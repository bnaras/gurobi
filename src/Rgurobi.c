
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <assert.h>
#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include "gurobi_c.h"

/* Error codes ------------------------------------------------------------- */

#define ERROR_OUT_OF_MEMORY      -10001
#define ERROR_UNKNOWN_GENCONSTR  -10002
#define ERROR_RELAX_MODEL        -10003
#define ERROR_PRESOLVE_MODEL     -10004

/* Aggregating duplicate triplets ------------------------------------------ */

#define ORDER_NONE         0
#define ORDER_FIRST_ONLY   1
#define ORDER_BOTH_STRICT  2


static int lexicographicordering(const int *row, const int *col, int nnzs)
{
  int both = 1;
  int nz;

  for (nz = 1; nz < nnzs; nz++) {
    if (row[nz - 1] == row[nz]) {
      if (both && col[nz - 1] >= col[nz]) /* Column order must be strict */
        both = 0;
    } else if (row[nz - 1] > row[nz]) {
      return ORDER_NONE;
    }
  }

  return (both ? ORDER_BOTH_STRICT : ORDER_FIRST_ONLY);
}

static int *buildcsrpointers(const int *row, int nrows, int nnzs)
{
  int *beg = NULL;
  int  nz, r;

  beg = (int *) calloc(nrows + 1, sizeof(int));
  if (beg == NULL) goto QUIT;

  /* Build cumulative row counts to define CSR pointers */

  for (nz = 0; nz < nnzs; nz++)
    beg[row[nz]]++;

  beg[nrows] = nnzs;
  for (r = nrows - 1; r >= 0; r--)
    beg[r] = beg[r + 1] - beg[r];

QUIT:

  return beg;
}

static int *buildroworder(const int *beg, const int *row, int nrows, int nnzs)
{
  int *next  = NULL;
  int *order = NULL;
  int  nz, r, o;

  /* Create copy of CSR pointers we can modify */

  next = (int *) malloc(nrows * sizeof(int));
  if (next == NULL) goto QUIT;

  memcpy(next, beg, nrows * sizeof(int));

  /* Build row order in O(nnzs), instead of O(nnzs * log nnzs) sort */

  order = (int *) malloc(nnzs * sizeof(int));
  if (order == NULL) goto QUIT;

  for (nz = 0; nz < nnzs; nz++) {
    r = row[nz];
    o = next[r]++;
    order[o] = nz;
  }

QUIT:

  free(next);

  return order;
}

static void aggregatevalues(double *val, const int *row, const int *col,
                            int nrows, int ncols, int nnzs)
{
  int *beg   = NULL;
  int *order = NULL;
  int *last  = NULL;
  int  r, o, nz, c;

  beg = buildcsrpointers(row, nrows, nnzs);
  if (beg == NULL) goto QUIT;

  order = buildroworder(beg, row, nrows, nnzs);
  if (order == NULL) goto QUIT;

  last = (int *) malloc(ncols * sizeof(int));
  if (last == NULL) goto QUIT;

  for (r = 0; r < nrows; r++) {

    /* Find last nonzero associated with each col in current row */

    for (o = beg[r]; o < beg[r + 1]; o++) {
      nz = order[o];
      c = col[nz];
      last[c] = nz;
    }

    /* Gurobi's C API has inconsistencies with respect to duplicate triplets:
     *   GRBchgcoeffs() ignores all duplicates except for the last one
     *   GRBaddqpterms() will use sum over all duplicates
     *   GRBaddqconstr() will use sum over all duplicates
     * To get consistent behavior (which mimics the default behavior in R
     * packages such as Matrix), we zero out all duplicates except for the
     * last one, which stores the sum over all duplicates
     */

    for (o = beg[r]; o < beg[r + 1]; o++) {
      nz = order[o];
      c = col[nz];
      if (last[c] != nz) {
        val[last[c]] += val[nz];
        val[nz] = 0.0;
      }
    }
  }

QUIT:

  free(beg);
  free(order);

  if (last == NULL)
    Rf_error("Out of memory\n");

  free(last);
}

static void aggregateorderedvalues(double *val, const int *row, const int *col,
                                   int ncols, int nnzs)
{
  int *last = NULL;
  int  beg, end, nz, c;

  last = (int *) malloc(ncols * sizeof(int));
  if (last == NULL)
    Rf_error("Out of memory\n");

  for (beg = 0; beg < nnzs; beg = end) {

    /* Find end of current row */

    for (end = beg + 1; end < nnzs; end++)
      if (row[beg] != row[end])
        break;

    /* Find last nonzero associated with each col in current row */

    for (nz = beg; nz < end; nz++) {
      c = col[nz];
      last[c] = nz;
    }

    /* Gurobi's C API has inconsistencies with respect to duplicate triplets:
     *   GRBchgcoeffs() ignores all duplicates except for the last one
     *   GRBaddqpterms() will use sum over all duplicates
     *   GRBaddqconstr() will use sum over all duplicates
     * To get consistent behavior (which mimics the default behavior in R
     * packages such as Matrix), we zero out all duplicates except for the
     * last one, which stores the sum over all duplicates
     */

    for (nz = beg; nz < end; nz++) {
      c = col[nz];
      if (last[c] != nz) {
        val[last[c]] += val[nz];
        val[nz] = 0.0;
      }
    }
  }

  free(last);
}

/* Querying SEXPs ---------------------------------------------------------- */

static SEXP getlistelem(SEXP list, const char *name)
{
  SEXP names = Rf_getAttrib(list, R_NamesSymbol);
  int  i;

  for (i = 0; i < Rf_length(names); i++)
    if (strcmp(CHAR(STRING_ELT(names, i)), name) == 0)
      return VECTOR_ELT(list, i);

  return R_NilValue;
}

static const char *getlistchr(SEXP list, const char *name, const char *nil)
{
  SEXP elem = getlistelem(list, name);
  return (Rf_length(elem) > 0 ? CHAR(STRING_ELT(elem, 0)) : nil);
}

static double getlistdbl(SEXP list, const char *name, double nil)
{
  SEXP elem = getlistelem(list, name);
  return (Rf_length(elem) > 0 ? REAL(elem)[0] : nil);
}

static int getlistint(SEXP list, const char *name, int nil)
{
  SEXP elem = getlistelem(list, name);
  return (Rf_length(elem) > 0 ? INTEGER(elem)[0] : nil);
}

static double *todbl(SEXP vector)
{
  return (Rf_length(vector) > 0 ? REAL(vector) : NULL);
}

static int *toint(SEXP vector)
{
  return (Rf_length(vector) > 0 ? INTEGER(vector) : NULL);
}

/* builds options string for function constraints */
static void buildoptionsstring(char *optionsstr,
                               int   len,
                               SEXP  list)
{
  SEXP elem;

  int pos = 0;          /* Next writable position within that string */

  /* if the funcpieces option is set add it ro the options string */
  elem = getlistelem(list, "funcpieces");
  if (Rf_length(elem) > 0) {
    pos += sprintf(optionsstr + pos, "FuncPieces=%d, ", INTEGER(elem)[0]);
  }
  assert(pos < len);

  /* if the funcpiecelength option is set add it ro the options string */
  elem = getlistelem(list, "funcpiecelength");
  if ( Rf_length(elem) > 0) {
    pos += snprintf(optionsstr + pos, len - pos, "FuncPieceLength=%g, ", REAL(elem)[0]);
  }
  assert(pos < len);

  /* if the funcpieceerror option is set add it ro the options string */
  elem = getlistelem(list, "funcpieceerror");
  if (Rf_length(elem) > 0) {
    pos += sprintf(optionsstr + pos, "FuncPieceError=%g, ", REAL(elem)[0]);
  }
  assert(pos < len);

  /* if the funcpieceratio option is set add it to the options string */
  elem = getlistelem(list, "funcpieceratio");
  if (Rf_length(elem) > 0) {
    pos += sprintf(optionsstr + pos, "FuncPieceRatio=%g, ", REAL(elem)[0]);
  }
  assert(pos < len);

  if (pos > 0) {
    /* We did write at least one option, reset to remove trailing ", " */
    pos = pos - 2;
  }
  assert(pos < len);

  /* Advance outer cursor to end of the string, terminate string */
  optionsstr[pos] = '\0';
}

/* Building GRBenv --------------------------------------------------------- */

static int setparam(GRBenv *env, SEXP args)
{
  const char *paramname = getlistchr(args, "paramname", "");
  const char *value     = getlistchr(args, "value", "");

  return GRBsetparam(env, paramname, value);
}

static int buildenv(GRBenv **envP, int *displayP, SEXP args)
{
  SEXP envargs       = getlistelem(args, "loadenv");
  SEXP csargs        = getlistelem(args, "loadclientenv");
  SEXP cloudargs     = getlistelem(args, "loadcloudenv");
  SEXP setparam_args = getlistelem(args, "setparam");
  int  i, outputflag, logtoconsole;
  int  error              = 0;

  /* Build environment */
  error = GRBemptyenvadv(envP, 5, GRB_VERSION_MAJOR, GRB_VERSION_MINOR,
                         GRB_VERSION_TECHNICAL, NULL, NULL, NULL, NULL, NULL, NULL, NULL);
  if (error) return error;

  /* load parameters first */
  for (i = 0; i < Rf_length(setparam_args); i++) {
    error = setparam(*envP, VECTOR_ELT(setparam_args, i));
    if (error) return error;
  }

  /* process environment settings */
  if (!Rf_isNull(envargs)) {
    const char *logfilename = getlistchr(envargs, "logfilename", NULL);
    GRBsetstrparam(*envP, GRB_STR_PAR_LOGFILE, logfilename);
  } else if (!Rf_isNull(csargs)) {
    const char *logfilename   = getlistchr(csargs, "logfilename", NULL);
    const char *servers       = getlistchr(csargs, "computeserver", NULL);
    const char *router        = getlistchr(csargs, "router", NULL);
    const char *group         = getlistchr(csargs, "group", NULL);
    const char *password      = getlistchr(csargs, "password", NULL);
    int         tlsinsecure   = getlistint(csargs, "cstlsinsecure", 0);
    int         priority      = getlistint(csargs, "priority", 0);
    GRBsetstrparam(*envP, GRB_STR_PAR_LOGFILE, logfilename);
    if (error) return error;
    GRBsetstrparam(*envP, GRB_STR_PAR_COMPUTESERVER, servers);
    GRBsetstrparam(*envP, GRB_STR_PAR_CSROUTER, router);
    GRBsetstrparam(*envP, GRB_STR_PAR_CSGROUP, group);
    if (password)
      GRBsetstrparam(*envP, GRB_STR_PAR_SERVERPASSWORD, password);
    GRBsetintparam(*envP, GRB_INT_PAR_CSTLSINSECURE, tlsinsecure);
    GRBsetintparam(*envP, GRB_INT_PAR_CSPRIORITY, priority);
  } else if (!Rf_isNull(cloudargs)) {
    const char *logfilename = getlistchr(cloudargs, "logfilename", NULL);
    const char *accessid    = getlistchr(cloudargs, "accessid", NULL);
    const char *secretkey   = getlistchr(cloudargs, "secretkey", NULL);
    const char *pool        = getlistchr(cloudargs, "pool", NULL);
    int         priority    = getlistint(cloudargs, "priority", 0);
    GRBsetstrparam(*envP, GRB_STR_PAR_LOGFILE, logfilename);
    GRBsetstrparam(*envP, GRB_STR_PAR_CLOUDACCESSID, accessid);
    GRBsetstrparam(*envP, GRB_STR_PAR_CLOUDSECRETKEY, secretkey);
    GRBsetstrparam(*envP, GRB_STR_PAR_CLOUDPOOL, pool);
    GRBsetintparam(*envP, GRB_INT_PAR_CSPRIORITY, priority);
  }

  /* Disable any logging to console (handled by displaycallback) */
  error = GRBgetintparam(*envP, GRB_INT_PAR_OUTPUTFLAG, &outputflag);
  if (error) return error;
  error = GRBgetintparam(*envP, GRB_INT_PAR_LOGTOCONSOLE, &logtoconsole);
  if (error) return error;
  *displayP = (outputflag && logtoconsole);
  error = GRBsetintparam(*envP, GRB_INT_PAR_LOGTOCONSOLE, 0);
  if (error) return error;

  /* start environment */
  error = GRBstartenv(*envP);
  if (error) return error;

  return 0;
}

/* Building GRBmodel ------------------------------------------------------- */

static int newmodel(GRBenv *env, GRBmodel **modelP, SEXP args)
{
  const char *name = getlistchr(args, "name", "R");

  return GRBnewmodel(env, modelP, name, 0, NULL, NULL, NULL, NULL, NULL);
}

static int __stdcall
displaycallback(GRBmodel *model, void *cbdata, int where, void *usrdata)
{
  char *msg;

  if (where == GRB_CB_MESSAGE) {
    GRBcbget(cbdata, where, GRB_CB_MSG_STRING, &msg);
    Rprintf("%s", msg);
  }

  return 0;
}

static int addvars(GRBmodel *model, SEXP args)
{
  int     numvars  = getlistint(args, "numvars", 0);
  double *obj      = todbl(getlistelem(args, "obj"));
  double *lb       = todbl(getlistelem(args, "lb"));
  double *ub       = todbl(getlistelem(args, "ub"));
  SEXP    vtype    = getlistelem(args, "vtype");
  SEXP    varnames = getlistelem(args, "varnames");
  int     v;
  int     error    = 0;

  error = GRBaddvars(model, numvars, 0, NULL, NULL,
                     NULL, obj, lb, ub, NULL, NULL);
  if (error) return error;

  error = GRBupdatemodel(model);
  if (error) return error;

  for (v = 0; v < Rf_length(vtype); v++) {
    error = GRBsetcharattrelement(model, GRB_CHAR_ATTR_VTYPE, v,
                                  CHAR(STRING_ELT(vtype, v))[0]);
    if (error) return error;
  }

  for (v = 0; v < Rf_length(varnames); v++) {
    error = GRBsetstrattrelement(model, GRB_STR_ATTR_VARNAME, v,
                                 CHAR(STRING_ELT(varnames, v)));
    if (error) return error;
  }

  return 0;
}

static int addconstrs(GRBmodel *model, SEXP args)
{
  int     numconstrs  = getlistint(args, "numconstrs", 0);
  SEXP    sense       = getlistelem(args, "sense");
  double *rhs         = todbl(getlistelem(args, "rhs"));
  SEXP    constrnames = getlistelem(args, "constrnames");
  int     c;
  int     error       = 0;

  error = GRBaddconstrs(model, numconstrs, 0, NULL, NULL,
                        NULL, NULL, rhs, NULL);
  if (error) return error;

  error = GRBupdatemodel(model);
  if (error) return error;

  for (c = 0; c < Rf_length(sense); c++) {
    error = GRBsetcharattrelement(model, GRB_CHAR_ATTR_SENSE, c,
                                  CHAR(STRING_ELT(sense, c))[0]);
    if (error) return error;
  }

  for (c = 0; c < Rf_length(constrnames); c++) {
    error = GRBsetstrattrelement(model, GRB_STR_ATTR_CONSTRNAME, c,
                                 CHAR(STRING_ELT(constrnames, c)));
    if (error) return error;
  }

  return 0;
}

static int chgcoeffs(GRBmodel *model, SEXP args)
{
  int     numchgs = getlistint(args, "numchgs", 0);
  int    *cind    = toint(getlistelem(args, "cind"));
  int    *vind    = toint(getlistelem(args, "vind"));
  double *val     = todbl(getlistelem(args, "val"));

  return GRBchgcoeffs(model, numchgs, cind, vind, val);
}

static int addqpterms(GRBmodel *model, SEXP args)
{
  int     numqnz = getlistint(args, "numqnz", 0);
  int    *qrow   = toint(getlistelem(args, "qrow"));
  int    *qcol   = toint(getlistelem(args, "qcol"));
  double *qval   = todbl(getlistelem(args, "qval"));

  return GRBaddqpterms(model, numqnz, qrow, qcol, qval);
}

static int addcone(GRBmodel *model, SEXP args)
{
  double      rhslb      = getlistdbl(args, "rhslb", 0.0);
  int         numqnz     = getlistint(args, "numqnz", 0);
  int        *qrow       = toint(getlistelem(args, "qrow"));
  int        *qcol       = toint(getlistelem(args, "qcol"));
  double     *qval       = todbl(getlistelem(args, "qval"));
  const char *constrname = getlistchr(args, "constrname", "");
  int         error      = 0;

  /* Implied lower bound of 0 on cone RHS variable */

  if (rhslb < 0.0) {
    error = GRBsetdblattrelement(model, GRB_DBL_ATTR_LB, qcol[0], 0.0);
    if (error) return error;
  }

  return GRBaddqconstr(model, 0, NULL, NULL, numqnz,
                       qrow, qcol, qval, '<', 0.0, constrname);
}

static int addqconstr(GRBmodel *model, SEXP args)
{
  int         numlnz     = getlistint(args, "numlnz", 0);
  int        *lind       = toint(getlistelem(args, "lind"));
  double     *lval       = todbl(getlistelem(args, "lval"));
  int         numqnz     = getlistint(args, "numqnz", 0);
  int        *qrow       = toint(getlistelem(args, "qrow"));
  int        *qcol       = toint(getlistelem(args, "qcol"));
  double     *qval       = todbl(getlistelem(args, "qval"));
  char        sense      = getlistchr(args, "sense", "<")[0];
  double      rhs        = getlistdbl(args, "rhs", 0.0);
  const char *constrname = getlistchr(args, "constrname", "");

  return GRBaddqconstr(model, numlnz, lind, lval, numqnz,
                       qrow, qcol, qval, sense, rhs, constrname);
}

static int addsos(GRBmodel *model, SEXP args)
{
  int     nummembers = getlistint(args, "nummembers", 0);
  int    *types      = toint(getlistelem(args, "types"));
  int     beg        = 0;
  int    *ind        = toint(getlistelem(args, "ind"));
  double *weight     = todbl(getlistelem(args, "weight"));

  return GRBaddsos(model, 1, nummembers, types, &beg, ind, weight);
}

static int setpwlobj(GRBmodel *model, SEXP args)
{
  int     var     = getlistint(args, "var", -1);
  int     npoints = getlistint(args, "npoints", 0);
  double *x       = todbl(getlistelem(args, "x"));
  double *y       = todbl(getlistelem(args, "y"));

  return GRBsetpwlobj(model, var, npoints, x, y);
}

static int addgenconstrMaxMin(int gctype, GRBmodel *model, SEXP args)
{
  const char *name     = getlistchr(args, "name", "");
  int         resvar   = getlistint(args, "resvar", -1);
  int         nvars    = getlistint(args, "nvars", 0);
  int        *vars     = toint(getlistelem(args, "vars"));
  double      constant;

  if (gctype == GRB_GENCONSTR_MAX) {
    constant = getlistdbl(args, "constant", -GRB_INFINITY);
    return GRBaddgenconstrMax(model, name, resvar, nvars, vars, constant);
  } else {
    constant = getlistdbl(args, "constant", GRB_INFINITY);
    return GRBaddgenconstrMin(model, name, resvar, nvars, vars, constant);
  }
}

static int addgenconstrAbs(GRBmodel *model, SEXP args)
{
  const char *name   = getlistchr(args, "name", "");
  int         resvar = getlistint(args, "resvar", -1);
  int         argvar = getlistint(args, "argvar", -1);

  return GRBaddgenconstrAbs(model, name, resvar, argvar);
}

static int addgenconstrAndOr(int gctype, GRBmodel *model, SEXP args)
{
  const char *name   = getlistchr(args, "name", "");
  int         resvar = getlistint(args, "resvar", -1);
  int         nvars  = getlistint(args, "nvars", 0);
  int        *vars   = toint(getlistelem(args, "vars"));

  if (gctype == GRB_GENCONSTR_AND)
    return GRBaddgenconstrAnd(model, name, resvar, nvars, vars);
  else
    return GRBaddgenconstrOr(model, name, resvar, nvars, vars);
}

static int addgenconstrInd(GRBmodel *model, SEXP args)
{
  const char *name   = getlistchr(args, "name", "");
  int         binvar = getlistint(args, "binvar", -1);
  int         binval = getlistint(args, "binval", -1);
  int         nvars  = getlistint(args, "nvars", 0);
  int        *ind    = toint(getlistelem(args, "ind"));
  double     *val    = todbl(getlistelem(args, "val"));
  char        sense  = getlistchr(args, "sense", "<")[0];
  double      rhs    = getlistdbl(args, "rhs", 0.0);

  return GRBaddgenconstrIndicator(model, name, binvar, binval,
                                  nvars, ind, val, sense, rhs);
}

static int addgenconstrPWL(GRBmodel *model, SEXP args)
{
  const char *name   = getlistchr(args, "name", "");
  int         xvar   = getlistint(args, "xvar", -1);
  int         yvar   = getlistint(args, "yvar", -1);
  int         npts   = getlistint(args, "npts", 0);
  double     *xpts   = todbl(getlistelem(args, "xpts"));
  double     *ypts   = todbl(getlistelem(args, "ypts"));

  return GRBaddgenconstrPWL(model, name, xvar, yvar, npts, xpts, ypts);
}

static int addgenconstrPoly(GRBmodel *model, SEXP args)
{
  const char *name    = getlistchr(args, "name", "");
  int         yvar    = getlistint(args, "yvar", -1);
  int         xvar    = getlistint(args, "xvar", -1);
  int         plen    = getlistint(args, "plen", 0);
  double     *p       = todbl(getlistelem(args, "p"));
  char        optionsstr[256];

  /* construct options string */
  buildoptionsstring(optionsstr, 256, args);

  return GRBaddgenconstrPoly(model, name, xvar, yvar, plen, p, optionsstr);
}

static int addgenconstrExpA(GRBmodel *model, SEXP args)
{
  const char *name    = getlistchr(args, "name", "");
  int         yvar    = getlistint(args, "yvar", -1);
  int         xvar    = getlistint(args, "xvar", -1);
  double      a       = getlistdbl(args, "a", 0);
  char        optionsstr[256];

  /* construct options string */
  buildoptionsstring(optionsstr, 256, args);

  return GRBaddgenconstrExpA(model, name, xvar, yvar, a, optionsstr);
}

static int addgenconstrLogA(GRBmodel *model, SEXP args)
{
  const char *name    = getlistchr(args, "name", "");
  int         yvar    = getlistint(args, "yvar", -1);
  int         xvar    = getlistint(args, "xvar", -1);
  double      a       = getlistdbl(args, "a", 0);
  char        optionsstr[256];

  /* construct options string */
  buildoptionsstring(optionsstr, 256, args);

  return GRBaddgenconstrLogA(model, name, xvar, yvar, a, optionsstr);
}

static int addgenconstrPow(GRBmodel *model, SEXP args)
{
  const char *name    = getlistchr(args, "name", "");
  int         yvar    = getlistint(args, "yvar", -1);
  int         xvar    = getlistint(args, "xvar", -1);
  double      a       = getlistdbl(args, "a", 0);
  char        optionsstr[256];

  /* construct options string */
  buildoptionsstring(optionsstr, 256, args);

  return GRBaddgenconstrPow(model, name, xvar, yvar, a, optionsstr);
}

static int addgenconstrExp(GRBmodel *model, SEXP args)
{
  const char *name    = getlistchr(args, "name", "");
  int         yvar    = getlistint(args, "yvar", -1);
  int         xvar    = getlistint(args, "xvar", -1);
  char        optionsstr[256];

  /* construct options string */
  buildoptionsstring(optionsstr, 256, args);

  return GRBaddgenconstrExp(model, name, xvar, yvar, optionsstr);
}

static int addgenconstrLog(GRBmodel *model, SEXP args)
{
  const char *name    = getlistchr(args, "name", "");
  int         yvar    = getlistint(args, "yvar", -1);
  int         xvar    = getlistint(args, "xvar", -1);
  char        optionsstr[256];

  /* construct options string */
  buildoptionsstring(optionsstr, 256, args);

  return GRBaddgenconstrLog(model, name, xvar, yvar, optionsstr);
}

static int addgenconstrSin(GRBmodel *model, SEXP args)
{
  const char *name = getlistchr(args, "name", "");
  int         yvar = getlistint(args, "yvar", -1);
  int         xvar = getlistint(args, "xvar", -1);
  char        optionsstr[256];

  /* construct options string */
  buildoptionsstring(optionsstr, 256, args);

  return GRBaddgenconstrSin(model, name, xvar, yvar, optionsstr);
}

static int addgenconstrCos(GRBmodel *model, SEXP args)
{
  const char *name    = getlistchr(args, "name", "");
  int         yvar    = getlistint(args, "yvar", -1);
  int         xvar    = getlistint(args, "xvar", -1);
  char        optionsstr[256];

  /* construct options string */
  buildoptionsstring(optionsstr, 256, args);

  return GRBaddgenconstrCos(model, name, xvar, yvar, optionsstr);
}

static int addgenconstrTan(GRBmodel *model, SEXP args)
{
  const char *name    = getlistchr(args, "name", "");
  int         yvar    = getlistint(args, "yvar", -1);
  int         xvar    = getlistint(args, "xvar", -1);
  char        optionsstr[256];

  /* construct options string */
  buildoptionsstring(optionsstr, 256, args);

  return GRBaddgenconstrTan(model, name, xvar, yvar, optionsstr);
}

static int setobjectiven(GRBmodel *model, int n, SEXP args)
{
  int         priority = getlistint(args, "priority", 0);
  double      weight   = getlistdbl(args, "weight", 1.0);
  double      abstol   = getlistdbl(args, "abstol", 0.0);
  double      reltol   = getlistdbl(args, "reltol", 0.0);
  const char *name     = getlistchr(args, "name", "");
  double      constant = getlistdbl(args, "constant", 0.0);
  int         lnz      = getlistint(args, "lnz", 0);
  int        *lind     = toint(getlistelem(args, "lind"));
  double     *lval     = todbl(getlistelem(args, "lval"));

  return GRBsetobjectiven(model, n, priority, weight, abstol, reltol,
                          name, constant, lnz, lind, lval);
}

static int setattr(GRBmodel *model, SEXP args)
{
  SEXP        modelsense     = getlistelem(args, "modelsense");
  SEXP        objcon         = getlistelem(args, "objcon");
  SEXP        start          = getlistelem(args, "start");
  SEXP        varhintval     = getlistelem(args, "varhintval");
  SEXP        varhintpri     = getlistelem(args, "varhintpri");
  SEXP        branchpriority = getlistelem(args, "branchpriority");
  SEXP        partition      = getlistelem(args, "partition");
  SEXP        vbasis         = getlistelem(args, "vbasis");
  SEXP        pstart         = getlistelem(args, "pstart");
  SEXP        cbasis         = getlistelem(args, "cbasis");
  SEXP        dstart         = getlistelem(args, "dstart");
  SEXP        lazy           = getlistelem(args, "lazy");
  SEXP        vtag           = getlistelem(args, "vtag");
  SEXP        ctag           = getlistelem(args, "ctag");

  const char *sense;
  int         error          = 0;
  int v;

  error = GRBupdatemodel(model);
  if (error) return error;

  /* Model attributes */

  if (Rf_length(modelsense) > 0) {
    sense = CHAR(STRING_ELT(modelsense, 0));
    if (strlen(sense) == 3       &&
        tolower(sense[0]) == 'm' &&
        tolower(sense[1]) == 'a' &&
        tolower(sense[2]) == 'x'   ) {
      error = GRBsetintattr(model, GRB_INT_ATTR_MODELSENSE, GRB_MAXIMIZE);
      if (error) return error;
    }
  }

  if (Rf_length(objcon) > 0) {
    error = GRBsetdblattr(model, GRB_DBL_ATTR_OBJCON, todbl(objcon)[0]);
    if (error) return error;
  }

  /* Variable attributes */

  if (Rf_length(start) > 0) {
    error = GRBsetdblattrarray(model, GRB_DBL_ATTR_START, 0,
                               Rf_length(start), todbl(start));
    if (error) return error;
  }

  if (Rf_length(varhintval) > 0) {
    error = GRBsetdblattrarray(model, GRB_DBL_ATTR_VARHINTVAL, 0,
                               Rf_length(varhintval), todbl(varhintval));
    if (error) return error;
  }

  if (Rf_length(varhintpri) > 0) {
    error = GRBsetintattrarray(model, GRB_INT_ATTR_VARHINTPRI, 0,
                               Rf_length(varhintpri), toint(varhintpri));
    if (error) return error;
  }

  if (Rf_length(branchpriority) > 0) {
    error = GRBsetintattrarray(model, GRB_INT_ATTR_BRANCHPRIORITY, 0,
                               Rf_length(branchpriority),
                               toint(branchpriority));
    if (error) return error;
  }

  if (Rf_length(partition) > 0) {
    error = GRBsetintattrarray(model, GRB_INT_ATTR_PARTITION, 0,
                               Rf_length(partition),
                               toint(partition));
    if (error) return error;
  }

  if (Rf_length(vbasis) > 0) {
    error = GRBsetintattrarray(model, GRB_INT_ATTR_VBASIS, 0,
                               Rf_length(vbasis), toint(vbasis));
    if (error) return error;
  }

  if (Rf_length(pstart) > 0) {
    error = GRBsetdblattrarray(model, GRB_DBL_ATTR_PSTART, 0,
                               Rf_length(pstart), todbl(pstart));
    if (error) return error;
  }

  if (Rf_length(vtag) > 0) {
    for (v = 0; v < Rf_length(vtag); v++) {
      error = GRBsetstrattrelement(model, GRB_STR_ATTR_VTAG, v,
                                   CHAR(STRING_ELT(vtag, v)));
      if (error) return error;
    }
  }

  /* Constraint attributes */

  if (Rf_length(cbasis) > 0) {
    error = GRBsetintattrarray(model, GRB_INT_ATTR_CBASIS, 0,
                               Rf_length(cbasis), toint(cbasis));
    if (error) return error;
  }

  if (Rf_length(dstart) > 0) {
    error = GRBsetdblattrarray(model, GRB_DBL_ATTR_DSTART, 0,
                               Rf_length(dstart), todbl(dstart));
    if (error) return error;
  }

  if (Rf_length(lazy) > 0) {
    error = GRBsetintattrarray(model, GRB_INT_ATTR_LAZY, 0,
                               Rf_length(lazy), toint(lazy));
    if (error) return error;
  }

  if (Rf_length(ctag) > 0) {
    for (v = 0; v < Rf_length(ctag); v++) {
      error = GRBsetstrattrelement(model, GRB_STR_ATTR_VTAG, v,
                                   CHAR(STRING_ELT(ctag, v)));
      if (error) return error;
    }
  }

  return 0;
}

static int buildmodel(GRBenv *env, GRBmodel **modelP, int display, SEXP args)
{
  SEXP newmodel_args         = getlistelem(args, "newmodel");
  SEXP addvars_args          = getlistelem(args, "addvars");
  SEXP addconstrs_args       = getlistelem(args, "addconstrs");
  SEXP chgcoeffs_args        = getlistelem(args, "chgcoeffs");
  SEXP addqpterms_args       = getlistelem(args, "addqpterms");
  SEXP addcone_args          = getlistelem(args, "addcone");
  SEXP addqconstr_args       = getlistelem(args, "addqconstr");
  SEXP addsos_args           = getlistelem(args, "addsos");
  SEXP setpwlobj_args        = getlistelem(args, "setpwlobj");
  SEXP addgenconstrMax_args  = getlistelem(args, "addgenconstrMax");
  SEXP addgenconstrMin_args  = getlistelem(args, "addgenconstrMin");
  SEXP addgenconstrAbs_args  = getlistelem(args, "addgenconstrAbs");
  SEXP addgenconstrAnd_args  = getlistelem(args, "addgenconstrAnd");
  SEXP addgenconstrOr_args   = getlistelem(args, "addgenconstrOr");
  SEXP addgenconstrInd_args  = getlistelem(args, "addgenconstrInd");
  SEXP addgenconstrPWL_args  = getlistelem(args, "addgenconstrPWL");
  SEXP addgenconstrPoly_args = getlistelem(args, "addgenconstrPoly");
  SEXP addgenconstrExpA_args = getlistelem(args, "addgenconstrExpA");
  SEXP addgenconstrLogA_args = getlistelem(args, "addgenconstrLogA");
  SEXP addgenconstrPow_args  = getlistelem(args, "addgenconstrPow");
  SEXP addgenconstrExp_args  = getlistelem(args, "addgenconstrExp");
  SEXP addgenconstrLog_args  = getlistelem(args, "addgenconstrLog");
  SEXP addgenconstrSin_args  = getlistelem(args, "addgenconstrSin");
  SEXP addgenconstrCos_args  = getlistelem(args, "addgenconstrCos");
  SEXP addgenconstrTan_args  = getlistelem(args, "addgenconstrTan");
  SEXP setobjectiven_args    = getlistelem(args, "setobjectiven");
  SEXP setattr_args          = getlistelem(args, "setattr");
  int  i, numobj;
  int  error                = 0;

  /* Create an empty model */

  if (!Rf_isNull(newmodel_args)) {
    error = newmodel(env, modelP, newmodel_args);
    if (error) return error;
  }

  /* Set display callback function */

  if (display) {
    error = GRBsetcallbackfunc(*modelP, displaycallback, NULL);
    if (error) return error;
  }

  /* Add variables */

  if (!Rf_isNull(addvars_args)) {
    error = addvars(*modelP, addvars_args);
    if (error) return error;
  }

  /* Add linear constraints */

  if (!Rf_isNull(addconstrs_args)) {
    error = addconstrs(*modelP, addconstrs_args);
    if (error) return error;
  }

  if (!Rf_isNull(chgcoeffs_args)) {
    error = chgcoeffs(*modelP, chgcoeffs_args);
    if (error) return error;
  }

  /* Add quadratic objective */

  if (!Rf_isNull(addqpterms_args)) {
    error = addqpterms(*modelP, addqpterms_args);
    if (error) return error;
  }

  /* Add second-order cones */

  for (i = 0; i < Rf_length(addcone_args); i++) {
    error = addcone(*modelP, VECTOR_ELT(addcone_args, i));
    if (error) return error;
  }

  /* Add quadratic constraints */

  for (i = 0; i < Rf_length(addqconstr_args); i++) {
    error = addqconstr(*modelP, VECTOR_ELT(addqconstr_args, i));
    if (error) return error;
  }

  /* Add SOS constraints */

  for (i = 0; i < Rf_length(addsos_args); i++) {
    error = addsos(*modelP, VECTOR_ELT(addsos_args, i));
    if (error) return error;
  }

  /* Add PWL objectives */

  for (i = 0; i < Rf_length(setpwlobj_args); i++) {
    error = setpwlobj(*modelP, VECTOR_ELT(setpwlobj_args, i));
    if (error) return error;
  }

  /* Add general constraints */

  for (i = 0; i < Rf_length(addgenconstrMax_args); i++) {
    error = addgenconstrMaxMin(GRB_GENCONSTR_MAX, *modelP,
                               VECTOR_ELT(addgenconstrMax_args, i));
    if (error) return error;
  }

  for (i = 0; i < Rf_length(addgenconstrMin_args); i++) {
    error = addgenconstrMaxMin(GRB_GENCONSTR_MIN, *modelP,
                               VECTOR_ELT(addgenconstrMin_args, i));
    if (error) return error;
  }

  for (i = 0; i < Rf_length(addgenconstrAbs_args); i++) {
    error = addgenconstrAbs(*modelP, VECTOR_ELT(addgenconstrAbs_args, i));
    if (error) return error;
  }

  for (i = 0; i < Rf_length(addgenconstrAnd_args); i++) {
    error = addgenconstrAndOr(GRB_GENCONSTR_AND, *modelP,
                              VECTOR_ELT(addgenconstrAnd_args, i));
    if (error) return error;
  }

  for (i = 0; i < Rf_length(addgenconstrOr_args); i++) {
    error = addgenconstrAndOr(GRB_GENCONSTR_OR, *modelP,
                              VECTOR_ELT(addgenconstrOr_args, i));
    if (error) return error;
  }

  for (i = 0; i < Rf_length(addgenconstrInd_args); i++) {
    error = addgenconstrInd(*modelP, VECTOR_ELT(addgenconstrInd_args, i));
    if (error) return error;
  }

  for (i = 0; i < Rf_length(addgenconstrPWL_args); i++) {
    error = addgenconstrPWL(*modelP, VECTOR_ELT(addgenconstrPWL_args, i));
    if (error) return error;
  }

  /* polynomial */
  for (i = 0; i < Rf_length(addgenconstrPoly_args); i++) {
    error = addgenconstrPoly(*modelP, VECTOR_ELT(addgenconstrPoly_args, i));
    if (error) return error;
  }

  /* y = exp_a(x) */
  for (i = 0; i < Rf_length(addgenconstrExpA_args); i++) {
    error = addgenconstrExpA(*modelP, VECTOR_ELT(addgenconstrExpA_args, i));
    if (error) return error;
  }

  /* y = log_a(x) */
  for (i = 0; i < Rf_length(addgenconstrLogA_args); i++) {
    error = addgenconstrLogA(*modelP, VECTOR_ELT(addgenconstrLogA_args, i));
    if (error) return error;
  }

  /* y = x^a */
  for (i = 0; i < Rf_length(addgenconstrPow_args); i++) {
    error = addgenconstrPow(*modelP, VECTOR_ELT(addgenconstrPow_args, i));
    if (error) return error;
  }

  /* y = exp(x) */
  for (i = 0; i < Rf_length(addgenconstrExp_args); i++) {
    error = addgenconstrExp(*modelP, VECTOR_ELT(addgenconstrExp_args, i));
    if (error) return error;
  }

  /* y = log(x) */
  for (i = 0; i < Rf_length(addgenconstrLog_args); i++) {
    error = addgenconstrLog(*modelP, VECTOR_ELT(addgenconstrLog_args, i));
    if (error) return error;
  }

  /* y = sin(x) */
  for (i = 0; i < Rf_length(addgenconstrSin_args); i++) {
    error = addgenconstrSin(*modelP, VECTOR_ELT(addgenconstrSin_args, i));
    if (error) return error;
  }

  /* y = cos(x) */
  for (i = 0; i < Rf_length(addgenconstrCos_args); i++) {
    error = addgenconstrCos(*modelP, VECTOR_ELT(addgenconstrCos_args, i));
    if (error) return error;
  }

  /* y = tan(x) */
  for (i = 0; i < Rf_length(addgenconstrTan_args); i++) {
    error = addgenconstrTan(*modelP, VECTOR_ELT(addgenconstrTan_args, i));
    if (error) return error;
  }

  /* Set multiple objectives */

  numobj = Rf_length(setobjectiven_args);
  if (numobj > 0) {
    error = GRBsetintattr(*modelP, GRB_INT_ATTR_NUMOBJ, numobj);
    if (error) return error;
    for (i = 0; i < numobj; i++) {
      error = setobjectiven(*modelP, i, VECTOR_ELT(setobjectiven_args, i));
      if (error) return error;
    }
  }

  /* Set any remaining attributes */

  if (!Rf_isNull(setattr_args)) {
    error = setattr(*modelP, setattr_args);
    if (error) return error;
  }

  /* Update model one last time */

  return GRBupdatemodel(*modelP);
}

/* Building SEXPs ---------------------------------------------------------- */

static SEXP setlistelem(SEXP list, const char *name, SEXP elem)
{
  SEXP names = Rf_getAttrib(list, R_NamesSymbol);
  int  i;

  for (i = 0; i < Rf_length(names); i++)
    if (strcmp(CHAR(STRING_ELT(names, i)), name) == 0)
      break;

  SET_VECTOR_ELT(list, i, elem); /* raises error if i == Rf_length(names) */

  return elem;
}

static SEXP alloclist(int len, const char **names)
{
  SEXP list = PROTECT(Rf_allocVector(VECSXP, len));
  SEXP strs;
  int  i;

  if (names != NULL) {
    strs = Rf_allocVector(STRSXP, len);
    Rf_namesgets(list, strs);

    for (i = 0; i < len; i++)
      SET_STRING_ELT(strs, i, Rf_mkChar(names[i]));
  }

  UNPROTECT(1);

  return list;
}

static SEXP alloclen1list(const char *name)
{
  if (name != NULL)
    return alloclist(1, &name);
  else
    return alloclist(1, NULL);
}

static SEXP populatedlistelems(SEXP list)
{
  SEXP names = Rf_getAttrib(list, R_NamesSymbol);
  SEXP newlist;
  SEXP newnames;
  int  len, i;

  len = 0;
  for (i = 0; i < Rf_length(list); i++)
    if (!Rf_isNull(VECTOR_ELT(list, i)))
      len++;

  newlist = PROTECT(Rf_allocVector(VECSXP, len));
  newnames = Rf_allocVector(STRSXP, len);
  Rf_namesgets(newlist, newnames);

  len = 0;
  for (i = 0; i < Rf_length(list); i++)
    if (!Rf_isNull(VECTOR_ELT(list, i))) {
      SET_VECTOR_ELT(newlist, len, VECTOR_ELT(list, i));
      SET_STRING_ELT(newnames, len, STRING_ELT(names, i));
      len++;
    }

  UNPROTECT(1);

  return newlist;
}

static SEXP mergesublists(SEXP listoflists)
{
  SEXP sublist;
  SEXP subnames;
  SEXP newlist;
  SEXP newnames;
  int  len, i, j;

  len = 0;
  for (i = 0; i < Rf_length(listoflists); i++)
    len += Rf_length(VECTOR_ELT(listoflists, i));

  newlist = PROTECT(Rf_allocVector(VECSXP, len));
  newnames = Rf_allocVector(STRSXP, len);
  Rf_namesgets(newlist, newnames);

  len = 0;
  for (i = 0; i < Rf_length(listoflists); i++) {
    sublist = VECTOR_ELT(listoflists, i);
    subnames = Rf_getAttrib(sublist, R_NamesSymbol);
    for (j = 0; j < Rf_length(sublist); j++) {
      SET_VECTOR_ELT(newlist, len, VECTOR_ELT(sublist, j));
      SET_STRING_ELT(newnames, len, STRING_ELT(subnames, j));
      len++;
    }
  }

  newlist = populatedlistelems(newlist);

  UNPROTECT(1);

  return newlist;
}

static SEXP allocchrvector(int len)
{
  return Rf_allocVector(STRSXP, len);
}

static SEXP allocdblvector(int len)
{
  return Rf_allocVector(REALSXP, len);
}

static SEXP allocintvector(int len)
{
  return Rf_allocVector(INTSXP, len);
}

static SEXP scalarstring(const char *str)
{
  return (str != NULL && strlen(str) > 0 ? Rf_mkString(str) : R_NilValue);
}

/* Querying GRBmodel ------------------------------------------------------- */

typedef struct _modelsizes
{
  int nvars;
  int nconstrs;
  int nnzs;
  int nqnzs;
  int nqconstrs;
  int nsos;
  int npwlobjs;
  int ngconstrs;
  int nmobjs; /* 0 if model has single objective */
} modelsizes;

static int getmodelsizes(modelsizes *sizes, GRBmodel *model)
{
  int ismultiobj;
  int error = 0;

  error = GRBgetintattr(model, GRB_INT_ATTR_NUMVARS, &(sizes->nvars));
  if (error) return error;
  error = GRBgetintattr(model, GRB_INT_ATTR_NUMCONSTRS, &(sizes->nconstrs));
  if (error) return error;
  error = GRBgetintattr(model, GRB_INT_ATTR_NUMNZS, &(sizes->nnzs));
  if (error) return error;
  error = GRBgetq(model, &(sizes->nqnzs), NULL, NULL, NULL);
  if (error) return error;
  error = GRBgetintattr(model, GRB_INT_ATTR_NUMQCONSTRS, &(sizes->nqconstrs));
  if (error) return error;
  error = GRBgetintattr(model, GRB_INT_ATTR_NUMSOS, &(sizes->nsos));
  if (error) return error;
  error = GRBgetintattr(model, GRB_INT_ATTR_NUMPWLOBJVARS, &(sizes->npwlobjs));
  if (error) return error;
  error = GRBgetintattr(model, GRB_INT_ATTR_NUMGENCONSTRS,
                        &(sizes->ngconstrs));
  if (error) return error;
  error = GRBgetintattr(model, GRB_INT_ATTR_IS_MULTIOBJ, &ismultiobj);
  if (error) return error;
  if (ismultiobj) {
    error = GRBgetintattr(model, GRB_INT_ATTR_NUMOBJ, &(sizes->nmobjs));
    if (error) return error;
  } else {
    sizes->nmobjs = 0;
  }

  return 0;
}

#define ARRAY_LENGTH(A)  (sizeof(A) / sizeof((A)[0]))

static double replaceInf(double x)
{
  if (x >= GRB_INFINITY)
    return R_PosInf;
  if (x <= -GRB_INFINITY)
    return R_NegInf;
  return x;
}

static int getvars(SEXP list, GRBmodel *model, const modelsizes *sizes)
{
  static const
  char *NAMES[] = { "obj", "lb", "ub", "vtype", "varnames" };
  static const
  int   LEN     = ARRAY_LENGTH(NAMES);

  int      nvars   = sizes->nvars;
  int      nmobjs  = sizes->nmobjs;
  SEXP     elem    = setlistelem(list, "getvars", alloclist(LEN, NAMES));
  double  *obj     = NULL;
  double  *lb      = todbl(setlistelem(elem, "lb", allocdblvector(nvars)));
  double  *ub      = todbl(setlistelem(elem, "ub", allocdblvector(nvars)));
  SEXP     vtype   = setlistelem(elem, "vtype", allocchrvector(nvars));
  SEXP     names   = setlistelem(elem, "varnames", allocchrvector(nvars));
  int      rmobj   = 1;
  int      rmlb    = 1;
  int      rmub    = 1;
  int      rmvtype = 1;
  char     cstr[2] = { '\0', '\0' };
  char    *chars   = NULL;
  char   **strs    = NULL;
  int      v;
  int      error   = 0;

  chars = (char *) malloc(nvars * sizeof(char));
  strs = (char **) malloc(nvars * sizeof(char *));
  if (chars == NULL || strs == NULL) {
    error = ERROR_OUT_OF_MEMORY;
    goto QUIT;
  }

  if (nmobjs == 0) {
    obj = todbl(setlistelem(elem, "obj", allocdblvector(nvars)));
    error = GRBgetdblattrarray(model, GRB_DBL_ATTR_OBJ, 0, nvars, obj);
    if (error) goto QUIT;
  }
  error = GRBgetdblattrarray(model, GRB_DBL_ATTR_LB, 0, nvars, lb);
  if (error) goto QUIT;
  error = GRBgetdblattrarray(model, GRB_DBL_ATTR_UB, 0, nvars, ub);
  if (error) goto QUIT;
  error = GRBgetcharattrarray(model, GRB_CHAR_ATTR_VTYPE, 0, nvars, chars);
  if (error) goto QUIT;
  error = GRBgetstrattrarray(model, GRB_STR_ATTR_VARNAME, 0, nvars, strs);
  if (error) goto QUIT;

  for (v = 0; v < nvars; v++) {
    if (obj != NULL && obj[v] != 0.0)
      rmobj = 0;
    lb[v] = replaceInf(lb[v]);
    if (lb[v] != 0.0)
      rmlb = 0;
    ub[v] = replaceInf(ub[v]);
    if (ub[v] != R_PosInf && !(ub[v] == 1.0 && chars[v] == GRB_BINARY))
      rmub = 0;
    if (chars[v] != GRB_CONTINUOUS)
      rmvtype = 0;
    cstr[0] = chars[v];
    SET_STRING_ELT(vtype, v, Rf_mkChar(cstr));
    SET_STRING_ELT(names, v, Rf_mkChar(strs[v]));
  }

  if (rmobj)
    setlistelem(elem, "obj", R_NilValue);
  if (rmlb)
    setlistelem(elem, "lb", R_NilValue);
  if (rmub)
    setlistelem(elem, "ub", R_NilValue);
  if (rmvtype)
    setlistelem(elem, "vtype", R_NilValue);

QUIT:

  free(chars);
  free(strs);

  return error;
}

static int getconstrs(SEXP list, GRBmodel *model, const modelsizes *sizes)
{
  static const
  char *NAMES[] = { "sense", "rhs", "constrnames" };
  static const
  int   LEN     = ARRAY_LENGTH(NAMES);

  int      nconstrs = sizes->nconstrs;
  SEXP     elem     = setlistelem(list, "getconstrs", alloclist(LEN, NAMES));
  SEXP     sense    = setlistelem(elem, "sense", allocchrvector(nconstrs));
  double  *rhs      = todbl(setlistelem(elem, "rhs",
                                        allocdblvector(nconstrs)));
  SEXP     names    = setlistelem(elem, "constrnames",
                                  allocchrvector(nconstrs));
  int      rmsense  = 1;
  int      rmrhs    = 1;
  char     cstr[2]  = { '\0', '\0' };
  char    *chars    = NULL;
  char   **strs     = NULL;
  int      c;
  int      error    = 0;

  chars = (char *) malloc(nconstrs * sizeof(char));
  strs = (char **) malloc(nconstrs * sizeof(char *));
  if (chars == NULL || strs == NULL) {
    error = ERROR_OUT_OF_MEMORY;
    goto QUIT;
  }

  error = GRBgetcharattrarray(model, GRB_CHAR_ATTR_SENSE, 0, nconstrs, chars);
  if (error) goto QUIT;
  error = GRBgetdblattrarray(model, GRB_DBL_ATTR_RHS, 0, nconstrs, rhs);
  if (error) goto QUIT;
  error = GRBgetstrattrarray(model, GRB_STR_ATTR_CONSTRNAME,
                             0, nconstrs, strs);
  if (error) goto QUIT;

  for (c = 0; c < nconstrs; c++) {
    if (chars[c] != '<')
      rmsense = 0;
    cstr[0] = chars[c];
    SET_STRING_ELT(sense, c, Rf_mkChar(cstr));
    rhs[c] = replaceInf(rhs[c]);
    if (rhs[c] != 0.0)
      rmrhs = 0;
    SET_STRING_ELT(names, c, Rf_mkChar(strs[c]));
  }

  if (rmsense)
    setlistelem(elem, "sense", R_NilValue);
  if (rmrhs)
    setlistelem(elem, "rhs", R_NilValue);

QUIT:

  free(chars);
  free(strs);

  return error;
}

static SEXP allocdoublelist()
{
  static const
  char *NAMES[] = { "i", "x", "length" };
  static const
  int   LEN     = ARRAY_LENGTH(NAMES);

  return alloclist(LEN, NAMES);
}

static SEXP alloctripletslist()
{
  static const
  char *NAMES[] = { "i", "j", "v", "nrow", "ncol" };
  static const
  int   LEN     = ARRAY_LENGTH(NAMES);

  return alloclist(LEN, NAMES);
}

static int getcoeffs(SEXP list, GRBmodel *model, const modelsizes *sizes)
{
  int     nconstrs = sizes->nconstrs;
  int     nvars    = sizes->nvars;
  int     nnzs     = sizes->nnzs;
  SEXP    elem     = setlistelem(list, "getcoeffs", alloclen1list("A"));
  SEXP    A        = setlistelem(elem, "A", alloctripletslist());
  int    *i        = toint(setlistelem(A, "i", allocintvector(nnzs)));
  int    *j        = toint(setlistelem(A, "j", allocintvector(nnzs)));
  double *v        = todbl(setlistelem(A, "v", allocdblvector(nnzs)));
  int    *beg      = NULL;
  int     c, nz;
  int     error    = 0;

  setlistelem(A, "nrow", Rf_ScalarInteger(nconstrs));
  setlistelem(A, "ncol", Rf_ScalarInteger(nvars));

  if (nnzs == 0) goto QUIT;

  beg = (int *) malloc((nconstrs + 1) * sizeof(int));
  if (beg == NULL) {
    error = ERROR_OUT_OF_MEMORY;
    goto QUIT;
  }

  error = GRBgetconstrs(model, &nnzs, beg, j, v, 0, nconstrs);
  if (error) goto QUIT;

  beg[nconstrs] = nnzs;
  for (c = 1; c <= nconstrs; c++) {
    for (nz = beg[c - 1]; nz < beg[c]; nz++) {
      i[nz] = c;
      j[nz]++;
    }
  }

QUIT:

  free(beg);

  return error;
}

static int getqpterms(SEXP list, GRBmodel *model, const modelsizes *sizes)
{
  int     nvars = sizes->nvars;
  int     nqnzs = sizes->nqnzs;
  SEXP    elem  = setlistelem(list, "getqpterms", alloclen1list("Q"));
  SEXP    Q     = setlistelem(elem, "Q", alloctripletslist());
  int    *i     = toint(setlistelem(Q, "i", allocintvector(nqnzs)));
  int    *j     = toint(setlistelem(Q, "j", allocintvector(nqnzs)));
  double *v     = todbl(setlistelem(Q, "v", allocdblvector(nqnzs)));
  int     nz;
  int     error = 0;

  setlistelem(Q, "nrow", Rf_ScalarInteger(nvars));
  setlistelem(Q, "ncol", Rf_ScalarInteger(nvars));

  error = GRBgetq(model, &nqnzs, i, j, v);
  if (error) return error;

  for (nz = 0; nz < nqnzs; nz++) {
    i[nz]++;
    j[nz]++;
  }

  return 0;
}

static int getqconstrs(SEXP list, GRBmodel *model, const modelsizes *sizes)
{
  static const
  char *NAMES[] = { "Qc", "q", "sense", "rhs", "name" };
  static const
  int   LEN     = ARRAY_LENGTH(NAMES);

  int      nvars     = sizes->nvars;
  int      nqconstrs = sizes->nqconstrs;
  SEXP     elem      = setlistelem(list, "getqconstrs",
                                   alloclen1list("quadcon"));
  SEXP     qclist    = setlistelem(elem, "quadcon",
                                   alloclist(nqconstrs, NULL));
  SEXP     qcelem    = R_NilValue;
  int     *lind      = NULL;
  double  *lval      = NULL;
  SEXP     q         = R_NilValue;
  SEXP     Qc        = R_NilValue;
  int     *i         = NULL;
  int     *j         = NULL;
  double  *v         = NULL;
  char     cstr[2]   = { '\0', '\0' };
  char    *qcsense   = NULL;
  double  *qcrhs     = NULL;
  char   **qcnames   = NULL;
  int      qc, nlnzs, nqnzs, nz;
  int      error     = 0;

  qcsense = (char *) malloc(nqconstrs * sizeof(char));
  qcrhs = (double *) malloc(nqconstrs * sizeof(double));
  qcnames = (char **) malloc(nqconstrs * sizeof(char *));
  if (qcsense == NULL || qcrhs == NULL || qcnames == NULL) {
    error = ERROR_OUT_OF_MEMORY;
    goto QUIT;
  }

  error = GRBgetcharattrarray(model, GRB_CHAR_ATTR_QCSENSE,
                              0, nqconstrs, qcsense);
  if (error) goto QUIT;
  error = GRBgetdblattrarray(model, GRB_DBL_ATTR_QCRHS, 0, nqconstrs, qcrhs);
  if (error) goto QUIT;
  error = GRBgetstrattrarray(model, GRB_STR_ATTR_QCNAME,
                             0, nqconstrs, qcnames);
  if (error) goto QUIT;

  for (qc = 0; qc < nqconstrs; qc++) {
    SET_VECTOR_ELT(qclist, qc, qcelem = alloclist(LEN, NAMES));

    /* get the number of non-zeros for the linear part and the quadratic
     * part
     */
    error = GRBgetqconstr(model, qc, &nlnzs, NULL, NULL,
                          &nqnzs, NULL, NULL, NULL);
    if (error) goto QUIT;

    if (nlnzs > 0) {
      q = setlistelem(qcelem, "q", allocdoublelist());
      lind = toint(setlistelem(q, "i", allocintvector(nlnzs)));
      lval = todbl(setlistelem(q, "x", allocdblvector(nlnzs)));
      setlistelem(q, "length", Rf_ScalarInteger(nvars));
    }

    Qc = setlistelem(qcelem, "Qc", alloctripletslist());
    i = toint(setlistelem(Qc, "i", allocintvector(nqnzs)));
    j = toint(setlistelem(Qc, "j", allocintvector(nqnzs)));
    v = todbl(setlistelem(Qc, "v", allocdblvector(nqnzs)));
    setlistelem(Qc, "nrow", Rf_ScalarInteger(nvars));
    setlistelem(Qc, "ncol", Rf_ScalarInteger(nvars));

    error = GRBgetqconstr(model, qc, &nlnzs, lind, lval, &nqnzs, i, j, v);
    if (error) goto QUIT;

    /* adjust indices to be 1-based (R uses 1-based indices) */
    for (nz = 0; nz < nlnzs; nz++)
      lind[nz]++;

    /* adjust indices to be 1-based (R uses 1-based indices) */
    for (nz = 0; nz < nqnzs; nz++) {
      i[nz]++;
      j[nz]++;
    }

    cstr[0] = qcsense[qc];
    if (qcsense[qc] != '<')
      setlistelem(qcelem, "sense", scalarstring(cstr));
    qcrhs[qc] = replaceInf(qcrhs[qc]);
    if (qcrhs[qc] != 0.0)
      setlistelem(qcelem, "rhs", Rf_ScalarReal(qcrhs[qc]));
    setlistelem(qcelem, "name", scalarstring(qcnames[qc]));

    SET_VECTOR_ELT(qclist, qc, populatedlistelems(qcelem));
  }

QUIT:

  free(qcsense);
  free(qcrhs);
  free(qcnames);

  return error;
}

static int getsos(SEXP list, GRBmodel *model, const modelsizes *sizes)
{
  static const
  char *NAMES[] = { "type", "index", "weight" };
  static const
  int   LEN     = ARRAY_LENGTH(NAMES);

  int     nsos    = sizes->nsos;
  SEXP    elem    = setlistelem(list, "getsos", alloclen1list("sos"));
  SEXP    soslist = setlistelem(elem, "sos", alloclist(nsos, NULL));
  SEXP    soselem = R_NilValue;
  int    *ind     = NULL;
  double *weight  = NULL;
  int     sos, nmembers, type, beg, i;
  int     error   = 0;

  for (sos = 0; sos < nsos; sos++) {
    SET_VECTOR_ELT(soslist, sos, soselem = alloclist(LEN, NAMES));

    error = GRBgetsos(model, &nmembers, &type, NULL, NULL, NULL, sos, 1);
    if (error) return error;

    ind = toint(setlistelem(soselem, "index", allocintvector(nmembers)));
    weight = todbl(setlistelem(soselem, "weight", allocdblvector(nmembers)));

    error = GRBgetsos(model, &nmembers, &type, &beg, ind, weight, sos, 1);
    if (error) return error;

    for (i = 0; i < nmembers; i++)
      ind[i]++;

    setlistelem(soselem, "type", Rf_ScalarInteger(type));
  }

  return 0;
}

static int getpwlobjs(SEXP list, GRBmodel *model, const modelsizes *sizes)
{
  static const
  char *NAMES[] = { "var", "x", "y" };
  static const
  int   LEN     = ARRAY_LENGTH(NAMES);

  int     nvars    = sizes->nvars;
  int     npwlobjs = sizes->npwlobjs;
  SEXP    elem     = setlistelem(list, "getpwlobjs", alloclen1list("pwlobj"));
  SEXP    pwllist  = setlistelem(elem, "pwlobj", alloclist(npwlobjs, NULL));
  SEXP    pwlelem  = R_NilValue;
  double *x        = NULL;
  double *y        = NULL;
  int     pwl, var, npoints;
  int     error    = 0;

  for (pwl = 0, var = 0; var < nvars; var++) {
    error = GRBgetpwlobj(model, var, &npoints, NULL, NULL);
    if (error) return error;

    if (npoints == 0) continue;

    SET_VECTOR_ELT(pwllist, pwl++, pwlelem = alloclist(LEN, NAMES));

    setlistelem(pwlelem, "var", Rf_ScalarInteger(var + 1));
    x = todbl(setlistelem(pwlelem, "x", allocdblvector(npoints)));
    y = todbl(setlistelem(pwlelem, "y", allocdblvector(npoints)));

    error = GRBgetpwlobj(model, var, &npoints, x, y);
    if (error) return error;
  }

  return 0;
}

static SEXP allocgenconstrlist(int gctype)
{
  static const
  char *MAXMIN_NAMES[]      = { "resvar", "vars", "con", "name" };
  static const
  char *ABS_NAMES[]         = { "resvar", "argvar", "name" };
  static const
  char *ANDOR_NAMES[]       = { "resvar", "vars", "name" };
  static const
  char *IND_NAMES[]         = { "binvar", "binval", "a", "sense", "rhs", "name" };
  static const
  char *PWL_NAMES[]         = { "xvar", "yvar", "xpts", "ypts", "name" };
  static const
  char *POLY_NAMES[]        = { "xvar", "yvar", "plen", "p", "name",
                                "funcpieces", "funcpiecelength",
                                "funcpieceerror", "funcpieceratio" };
  static const
  char *TWOVARSBASE_NAMES[] = { "xvar", "yvar", "a", "name",
                                "funcpieces", "funcpiecelength",
                                "funcpieceerror", "funcpieceratio" };
  static const
  char *TWOVARS_NAMES[]     = { "xvar", "yvar", "name",
                                "funcpieces", "funcpiecelength",
                                "funcpieceerror", "funcpieceratio" };

  switch (gctype) {
    case GRB_GENCONSTR_MAX:
    case GRB_GENCONSTR_MIN:
      return alloclist(ARRAY_LENGTH(MAXMIN_NAMES), MAXMIN_NAMES);
    case GRB_GENCONSTR_ABS:
      return alloclist(ARRAY_LENGTH(ABS_NAMES), ABS_NAMES);
    case GRB_GENCONSTR_AND:
    case GRB_GENCONSTR_OR:
      return alloclist(ARRAY_LENGTH(ANDOR_NAMES), ANDOR_NAMES);
    case GRB_GENCONSTR_INDICATOR:
      return alloclist(ARRAY_LENGTH(IND_NAMES), IND_NAMES);
    case GRB_GENCONSTR_PWL:
      return alloclist(ARRAY_LENGTH(PWL_NAMES), PWL_NAMES);
    case GRB_GENCONSTR_POLY:
      return alloclist(ARRAY_LENGTH(POLY_NAMES), POLY_NAMES);
    case GRB_GENCONSTR_EXPA:
    case GRB_GENCONSTR_LOGA:
    case GRB_GENCONSTR_POW:
      return alloclist(ARRAY_LENGTH(TWOVARSBASE_NAMES), TWOVARSBASE_NAMES);
    case GRB_GENCONSTR_EXP:
    case GRB_GENCONSTR_LOG:
    case GRB_GENCONSTR_SIN:
    case GRB_GENCONSTR_COS:
    case GRB_GENCONSTR_TAN:
      return alloclist(ARRAY_LENGTH(TWOVARS_NAMES), TWOVARS_NAMES);
    default:
      return R_NilValue;
  }
}

/* collect for the function constraint the options and them to the R structure */
static int addFunctionConstraintOptions(SEXP      gcelem,
                                        GRBmodel *model,
                                        int       gc)
{
  double dblvalue;
  int    intvalue;

  int error = 0;

  error = GRBgetintattrelement(model, GRB_INT_ATTR_FUNCPIECES, gc, &intvalue);
  if (error) goto QUIT;

  setlistelem(gcelem, "funcpieces", Rf_ScalarInteger(intvalue));

  error = GRBgetdblattrelement(model, GRB_DBL_ATTR_FUNCPIECELENGTH, gc, &dblvalue);
  if (error) goto QUIT;

  setlistelem(gcelem, "funcpiecelength", Rf_ScalarReal(dblvalue));

  error = GRBgetdblattrelement(model, GRB_DBL_ATTR_FUNCPIECEERROR, gc, &dblvalue);
  if (error) goto QUIT;

  setlistelem(gcelem, "funcpieceerror", Rf_ScalarReal(dblvalue));

  error = GRBgetdblattrelement(model, GRB_DBL_ATTR_FUNCPIECERATIO, gc, &dblvalue);
  if (error) goto QUIT;

  setlistelem(gcelem, "funcpieceratio", Rf_ScalarReal(dblvalue));

QUIT:

  return error;
}

static int getgenconstrs(SEXP list, GRBmodel *model, const modelsizes *sizes)
{
  /* the order of the general constraint names need to match the order of
   * the general constraint defines in api/public.h
   */
  static const
  char *NAMES[] = { "genconmax", "genconmin", "genconabs",
                    "genconand", "genconor", "genconind", "genconpwl",
                    "genconpoly","genconexp", "genconexpa", "genconlog", "genconloga",
                    "genconpow", "genconsin", "genconcos", "gencontan" };
  static const
  int   LEN     = ARRAY_LENGTH(NAMES);

  int      nvars           = sizes->nvars;
  int      ngconstrs       = sizes->ngconstrs;
  SEXP     elem            = setlistelem(list, "getgenconstrs",
                                         alloclist(LEN, NAMES));
  SEXP     gctypelist[LEN];
  SEXP     gcelem          = R_NilValue;
  int      ngctype[LEN];
  int     *gctypes         = NULL;
  char   **gcnames         = NULL;
  int     *vars            = NULL;
  double  *xpts            = NULL;
  double  *ypts            = NULL;
  double  *p               = NULL;
  int      xvar, yvar, npts;
  int      gc, t, ngcvars, v, resvar, argvar, binvar, binval, nanzs;
  int      plen;
  double   defaultcon, con, rhs, a;
  char     cstr[2]      = { '\0', '\0' };
  char     sense;
  int      error        = 0;

  memset(ngctype, 0, LEN * sizeof(int));

  gctypes = (int *) malloc(ngconstrs * sizeof(int));
  gcnames = (char **) malloc(ngconstrs * sizeof(char *));
  if (gctypes == NULL || gcnames == NULL) {
    error = ERROR_OUT_OF_MEMORY;
    goto QUIT;
  }

  error = GRBgetintattrarray(model, GRB_INT_ATTR_GENCONSTRTYPE,
                             0, ngconstrs, gctypes);
  if (error) goto QUIT;
  error = GRBgetstrattrarray(model, GRB_STR_ATTR_GENCONSTRNAME,
                             0, ngconstrs, gcnames);
  if (error) goto QUIT;

  for (gc = 0; gc < ngconstrs; gc++) {
    t = gctypes[gc];
    if (t < LEN) {
      ngctype[t]++;
    } else {
      error = ERROR_UNKNOWN_GENCONSTR;
      goto QUIT;
    }
  }

  for (t = 0; t < LEN; t++)
    if (ngctype[t] > 0)
      gctypelist[t] = setlistelem(elem, NAMES[t], alloclist(ngctype[t], NULL));

  memset(ngctype, 0, LEN * sizeof(int));

  for (gc = 0; gc < ngconstrs; gc++) {
    t = gctypes[gc];
    SET_VECTOR_ELT(gctypelist[t], ngctype[t], gcelem = allocgenconstrlist(t));

    switch (t) {
      case GRB_GENCONSTR_MAX:
      case GRB_GENCONSTR_MIN:

        if (t == GRB_GENCONSTR_MAX) {
          defaultcon = -GRB_INFINITY;
          error = GRBgetgenconstrMax(model, gc, &resvar, &ngcvars, NULL, &con);
        } else {
          defaultcon = GRB_INFINITY;
          error = GRBgetgenconstrMin(model, gc, &resvar, &ngcvars, NULL, &con);
        }
        if (error) goto QUIT;

        setlistelem(gcelem, "resvar", Rf_ScalarInteger(resvar + 1));
        vars = toint(setlistelem(gcelem, "vars", allocintvector(ngcvars)));
        if (con != defaultcon) {
          con = replaceInf(con);
          setlistelem(gcelem, "con", Rf_ScalarReal(con));
        }
        setlistelem(gcelem, "name", scalarstring(gcnames[gc]));

        if (t == GRB_GENCONSTR_MAX)
          error = GRBgetgenconstrMax(model, gc, &resvar, &ngcvars, vars, &con);
        else
          error = GRBgetgenconstrMin(model, gc, &resvar, &ngcvars, vars, &con);
        if (error) goto QUIT;

        for (v = 0; v < ngcvars; v++)
          vars[v]++;

        break;

      case GRB_GENCONSTR_ABS:

        error = GRBgetgenconstrAbs(model, gc, &resvar, &argvar);
        if (error) goto QUIT;

        setlistelem(gcelem, "resvar", Rf_ScalarInteger(resvar + 1));
        setlistelem(gcelem, "argvar", Rf_ScalarInteger(argvar + 1));
        setlistelem(gcelem, "name", scalarstring(gcnames[gc]));

        break;

      case GRB_GENCONSTR_AND:
      case GRB_GENCONSTR_OR:

        if (t == GRB_GENCONSTR_AND)
          error = GRBgetgenconstrAnd(model, gc, &resvar, &ngcvars, NULL);
        else
          error = GRBgetgenconstrOr(model, gc, &resvar, &ngcvars, NULL);
        if (error) goto QUIT;

        setlistelem(gcelem, "resvar", Rf_ScalarInteger(resvar + 1));
        vars = toint(setlistelem(gcelem, "vars", allocintvector(ngcvars)));
        setlistelem(gcelem, "name", scalarstring(gcnames[gc]));

        if (t == GRB_GENCONSTR_AND)
          error = GRBgetgenconstrAnd(model, gc, &resvar, &ngcvars, vars);
        else
          error = GRBgetgenconstrOr(model, gc, &resvar, &ngcvars, vars);
        if (error) goto QUIT;

        for (v = 0; v < ngcvars; v++)
          vars[v]++;

        break;

      case GRB_GENCONSTR_INDICATOR: {
        SEXP    a    = R_NilValue;
        double *aval = NULL;
        int    *aind = NULL;
        int     nz;

        error = GRBgetgenconstrIndicator(model, gc, &binvar, &binval,
                                         &nanzs, NULL, NULL, &sense, &rhs);
        if (error) goto QUIT;

        if (nanzs > 0) {
          a = setlistelem(gcelem, "a", allocdoublelist());
          aind = toint(setlistelem(a, "i", allocintvector(nanzs)));
          aval = todbl(setlistelem(a, "x", allocdblvector(nanzs)));
          setlistelem(a, "length", Rf_ScalarInteger(nvars));
        }

        error = GRBgetgenconstrIndicator(model, gc, &binvar, &binval,
                                         &nanzs, aind, aval, &sense, &rhs);

        /* adjust indices to be 1-based (R uses 1-based indices) */
        for (nz = 0; nz < nanzs; nz++)
          aind[nz]++;

        setlistelem(gcelem, "binvar", Rf_ScalarInteger(binvar + 1));
        setlistelem(gcelem, "binval", Rf_ScalarInteger(binval));
        cstr[0] = sense;
        if (sense != '<')
          setlistelem(gcelem, "sense", scalarstring(cstr));
        rhs = replaceInf(rhs);
        if (rhs != 0.0)
          setlistelem(gcelem, "rhs", Rf_ScalarReal(rhs));
        setlistelem(gcelem, "name", scalarstring(gcnames[gc]));

        break;
      }
      case GRB_GENCONSTR_PWL:

        error = GRBgetgenconstrPWL(model, gc, &xvar, &yvar, &npts,
                                   NULL, NULL);
        if (error) goto QUIT;

        if (npts == 0) continue;

        setlistelem(gcelem, "xvar", Rf_ScalarInteger(xvar+1));
        setlistelem(gcelem, "yvar", Rf_ScalarInteger(yvar+1));

        xpts = todbl(setlistelem(gcelem, "xpts", allocdblvector(npts)));
        ypts = todbl(setlistelem(gcelem, "ypts", allocdblvector(npts)));
        setlistelem(gcelem, "name", scalarstring(gcnames[gc]));

        error = GRBgetgenconstrPWL(model, gc, &xvar, &yvar, &npts, xpts, ypts);
        if (error) goto QUIT;

        break;

      case GRB_GENCONSTR_POLY:

        error = GRBgetgenconstrPoly(model, gc, &xvar, &yvar, &plen, NULL);
        if (error) goto QUIT;

        if (plen == 0) continue;

        setlistelem(gcelem, "xvar", Rf_ScalarInteger(xvar+1));
        setlistelem(gcelem, "yvar", Rf_ScalarInteger(yvar+1));

        p = todbl(setlistelem(gcelem, "p", allocdblvector(plen)));
        setlistelem(gcelem, "name", scalarstring(gcnames[gc]));

        error = GRBgetgenconstrPoly(model, gc, &xvar, &yvar, &plen, p);
        if (error) goto QUIT;

        /* collect and add the function constraints options to R data
         * structure
         */
        error = addFunctionConstraintOptions(gcelem, model, gc);
        if (error) goto QUIT;

        break;

      case GRB_GENCONSTR_EXPA:
      case GRB_GENCONSTR_LOGA:
      case GRB_GENCONSTR_POW:

        if (t == GRB_GENCONSTR_EXPA)
          error = GRBgetgenconstrExpA(model, gc, &xvar, &yvar, &a);
        else if (t == GRB_GENCONSTR_LOGA)
          error = GRBgetgenconstrLogA(model, gc, &xvar, &yvar, &a);
        else
          error = GRBgetgenconstrPow(model, gc, &xvar, &yvar, &a);
        if (error) goto QUIT;

        setlistelem(gcelem, "xvar", Rf_ScalarInteger(xvar+1));
        setlistelem(gcelem, "yvar", Rf_ScalarInteger(yvar+1));
        setlistelem(gcelem, "a", Rf_ScalarReal(a));
        setlistelem(gcelem, "name", scalarstring(gcnames[gc]));

        /* collect and add the function constraints options to R data
         * structure
         */
        error = addFunctionConstraintOptions(gcelem, model, gc);
        if (error) goto QUIT;

        break;

      case GRB_GENCONSTR_EXP:
      case GRB_GENCONSTR_LOG:
      case GRB_GENCONSTR_SIN:
      case GRB_GENCONSTR_COS:
      case GRB_GENCONSTR_TAN:

        if (t == GRB_GENCONSTR_EXP)
          error = GRBgetgenconstrExp(model, gc, &xvar, &yvar);
        else if (t == GRB_GENCONSTR_LOG)
          error = GRBgetgenconstrLog(model, gc, &xvar, &yvar);
        else if (t == GRB_GENCONSTR_SIN)
          error = GRBgetgenconstrSin(model, gc, &xvar, &yvar);
        else if (t == GRB_GENCONSTR_COS)
          error = GRBgetgenconstrCos(model, gc, &xvar, &yvar);
        else
          error = GRBgetgenconstrTan(model, gc, &xvar, &yvar);
        if (error) goto QUIT;

        setlistelem(gcelem, "xvar", Rf_ScalarInteger(xvar+1));
        setlistelem(gcelem, "yvar", Rf_ScalarInteger(yvar+1));
        setlistelem(gcelem, "name", scalarstring(gcnames[gc]));

        /* collect and add the function constraints options to R data
         * structure
         */
        error = addFunctionConstraintOptions(gcelem, model, gc);
        if (error) goto QUIT;

        break;

      default:

        error = ERROR_UNKNOWN_GENCONSTR;
        goto QUIT;
    }

    SET_VECTOR_ELT(gctypelist[t], ngctype[t]++, populatedlistelems(gcelem));
  }

QUIT:

  free(gctypes);
  free(gcnames);

  return error;
}

static int getobjectiven(SEXP list, GRBmodel *model, const modelsizes *sizes)
{
  static const
  char *NAMES[] = { "objn", "objcon", "priority", "weight",
                    "reltol", "abstol", "name" };
  static const
  int   LEN     = ARRAY_LENGTH(NAMES);

  int     nvars   = sizes->nvars;
  int     nmobjs  = sizes->nmobjs;
  SEXP    elem    = setlistelem(list, "getobjectiven",
                                alloclen1list("multiobj"));
  SEXP    objlist = setlistelem(elem, "multiobj", alloclist(nmobjs, NULL));
  SEXP    objelem = R_NilValue;
  double *objn    = NULL;
  int     obj, priority;
  double  objcon, weight, reltol, abstol;
  char   *name;
  int     error   = 0;

  for (obj = 0; obj < nmobjs; obj++) {
    SET_VECTOR_ELT(objlist, obj, objelem = alloclist(LEN, NAMES));

    objn = todbl(setlistelem(objelem, "objn", allocdblvector(nvars)));

    error = GRBsetintparam(GRBgetenv(model), GRB_INT_PAR_OBJNUMBER, obj);
    if (error) return error;
    error = GRBgetdblattrarray(model, GRB_DBL_ATTR_OBJN, 0, nvars, objn);
    if (error) return error;
    error = GRBgetdblattr(model, GRB_DBL_ATTR_OBJNCON, &objcon);
    if (error) return error;
    error = GRBgetintattr(model, GRB_INT_ATTR_OBJNPRIORITY, &priority);
    if (error) return error;
    error = GRBgetdblattr(model, GRB_DBL_ATTR_OBJNWEIGHT, &weight);
    if (error) return error;
    error = GRBgetdblattr(model, GRB_DBL_ATTR_OBJNRELTOL, &reltol);
    if (error) return error;
    error = GRBgetdblattr(model, GRB_DBL_ATTR_OBJNABSTOL, &abstol);
    if (error) return error;
    error = GRBgetstrattr(model, GRB_STR_ATTR_OBJNNAME, &name);
    if (error) return error;

    if (objcon != 0.0)
      setlistelem(objelem, "objcon", Rf_ScalarReal(objcon));
    if (priority != 0)
      setlistelem(objelem, "priority", Rf_ScalarInteger(priority));
    if (weight != 1.0)
      setlistelem(objelem, "weight", Rf_ScalarReal(weight));
    if (reltol != 0.0)
      setlistelem(objelem, "reltol", Rf_ScalarReal(reltol));
    if (abstol != 0.0)
      setlistelem(objelem, "abstol", Rf_ScalarReal(abstol));
    setlistelem(objelem, "name", scalarstring(name));

    SET_VECTOR_ELT(objlist, obj, populatedlistelems(objelem));
  }

  return 0;
}

static int getattr(SEXP list, GRBmodel *model, const modelsizes *sizes)
{
  static const
  char *NAMES[] = { "modelname", "modelsense", "objcon", "lazy" };
  static const
  int   LEN     = ARRAY_LENGTH(NAMES);

  int     nconstrs = sizes->nconstrs;
  int     nmobjs   = sizes->nmobjs;
  SEXP    elem     = setlistelem(list, "getattr", alloclist(LEN, NAMES));
  int    *lazy     = toint(setlistelem(elem, "lazy",
                                       allocintvector(nconstrs)));
  int     rmlazy   = 1;
  int     modelsense, c;
  double  objcon   = 0.0;
  char   *name;
  int     error    = 0;

  /* Model attributes */

  error = GRBgetstrattr(model, GRB_STR_ATTR_MODELNAME, &name);
  if (error) return error;
  error = GRBgetintattr(model, GRB_INT_ATTR_MODELSENSE, &modelsense);
  if (error) return error;
  if (nmobjs == 0) {
    error = GRBgetdblattr(model, GRB_DBL_ATTR_OBJCON, &objcon);
    if (error) return error;
  }

  setlistelem(elem, "modelname", scalarstring(name));
  if (modelsense == GRB_MAXIMIZE)
    setlistelem(elem, "modelsense", scalarstring("max"));
  if (objcon != 0.0)
    setlistelem(elem, "objcon", Rf_ScalarReal(objcon));

  /* Constraint attributes */

  error = GRBgetintattrarray(model, GRB_INT_ATTR_LAZY, 0, nconstrs, lazy);
  if (error) return error;

  /* Remove optional attributes set to defaults */

  for (c = 0; c < nconstrs; c++)
    if (lazy[c] != 0) {
      rmlazy = 0;
      break;
    }

  if (rmlazy)
    setlistelem(elem, "lazy", R_NilValue);

  return 0;
}

static int querymodel(SEXP *listP, GRBmodel *model)
{
  static const
  char *NAMES[] = { "getcoeffs", "getvars", "getconstrs", "getqpterms",
                    "getqconstrs", "getsos", "getpwlobjs", "getgenconstrs",
                    "getobjectiven", "getattr" };
  static const
  int   LEN     = ARRAY_LENGTH(NAMES);

  modelsizes sizes;
  int        error = 0;

  *listP = PROTECT(alloclist(LEN, NAMES));

  /* Get model sizes */

  error = getmodelsizes(&sizes, model);
  if (error) goto QUIT;

  /* Get variables */

  if (sizes.nvars > 0) {
    error = getvars(*listP, model, &sizes);
    if (error) goto QUIT;
  }

  /* Get linear constraints */

  if (sizes.nconstrs > 0) {
    error = getconstrs(*listP, model, &sizes);
    if (error) goto QUIT;
  }

  error = getcoeffs(*listP, model, &sizes);
  if (error) goto QUIT;

  /* Get quadratic objective */

  if (sizes.nqnzs > 0) {
    error = getqpterms(*listP, model, &sizes);
    if (error) goto QUIT;
  }

  /* Get quadratic constraints */

  if (sizes.nqconstrs > 0) {
    error = getqconstrs(*listP, model, &sizes);
    if (error) goto QUIT;
  }

  /* Get SOS constraints */

  if (sizes.nsos > 0) {
    error = getsos(*listP, model, &sizes);
    if (error) goto QUIT;
  }

  /* Get PWL objectives */

  if (sizes.npwlobjs > 0) {
    error = getpwlobjs(*listP, model, &sizes);
    if (error) goto QUIT;
  }

  /* Get general constraints */

  if (sizes.ngconstrs > 0) {
    error = getgenconstrs(*listP, model, &sizes);
    if (error) goto QUIT;
  }

  /* Get multiple objectives */

  if (sizes.nmobjs > 0) {
    error = getobjectiven(*listP, model, &sizes);
    if (error) goto QUIT;
  }

  /* Get any remaining attributes */

  error = getattr(*listP, model, &sizes);
  if (error) goto QUIT;

QUIT:

  *listP = mergesublists(*listP);

  UNPROTECT(1);

  return error;
}

/* Querying results -------------------------------------------------------- */

typedef struct _resultsizes
{
  int nvars;
  int nconstrs;
  int nqconstrs;
  int nmobjs; /* 0 if model has single objective */
  int nsols;
} resultsizes;

static int getresultsizes(resultsizes *sizes, GRBmodel *model)
{
  int ismultiobj;
  int error = 0;

  error = GRBgetintattr(model, GRB_INT_ATTR_NUMVARS, &(sizes->nvars));
  if (error) return error;
  error = GRBgetintattr(model, GRB_INT_ATTR_NUMCONSTRS, &(sizes->nconstrs));
  if (error) return error;
  error = GRBgetintattr(model, GRB_INT_ATTR_NUMQCONSTRS, &(sizes->nqconstrs));
  if (error) return error;
  error = GRBgetintattr(model, GRB_INT_ATTR_IS_MULTIOBJ, &ismultiobj);
  if (error) return error;
  if (ismultiobj) {
    error = GRBgetintattr(model, GRB_INT_ATTR_NUMOBJ, &(sizes->nmobjs));
    if (error) return error;
  } else {
    sizes->nmobjs = 0;
  }
  error = GRBgetintattr(model, GRB_INT_ATTR_SOLCOUNT, &(sizes->nsols));
  if (error) return error;

  return 0;
}

static int getoptstats(SEXP list, GRBmodel *model)
{
  static const
  char *NAMES[] = { "status", "runtime",
                    "itercount", "baritercount", "nodecount" };
  static const
  int   LEN     = ARRAY_LENGTH(NAMES);

  SEXP   elem         = setlistelem(list, "getoptstats",
                                    alloclist(LEN, NAMES));
  int    status;
  double runtime      = 0.0;
  double itercount    = 0.0;
  int    baritercount = 0;
  double nodecount    = 0.0;
  int    error        = 0;

  error = GRBgetintattr(model, GRB_INT_ATTR_STATUS, &status);
  if (error) return error;

  switch (status) {
    case GRB_LOADED:
      setlistelem(elem, "status", scalarstring("LOADED"));
      break;
    case GRB_OPTIMAL:
      setlistelem(elem, "status", scalarstring("OPTIMAL"));
      break;
    case GRB_INFEASIBLE:
      setlistelem(elem, "status", scalarstring("INFEASIBLE"));
      break;
    case GRB_INF_OR_UNBD:
      setlistelem(elem, "status", scalarstring("INF_OR_UNBD"));
      break;
    case GRB_UNBOUNDED:
      setlistelem(elem, "status", scalarstring("UNBOUNDED"));
      break;
    case GRB_CUTOFF:
      setlistelem(elem, "status", scalarstring("CUTOFF"));
      break;
    case GRB_ITERATION_LIMIT:
      setlistelem(elem, "status", scalarstring("ITERATION_LIMIT"));
      break;
    case GRB_NODE_LIMIT:
      setlistelem(elem, "status", scalarstring("NODE_LIMIT"));
      break;
    case GRB_TIME_LIMIT:
      setlistelem(elem, "status", scalarstring("TIME_LIMIT"));
      break;
    case GRB_SOLUTION_LIMIT:
      setlistelem(elem, "status", scalarstring("SOLUTION_LIMIT"));
      break;
    case GRB_INTERRUPTED:
      setlistelem(elem, "status", scalarstring("INTERRUPTED"));
      break;
    case GRB_NUMERIC:
      setlistelem(elem, "status", scalarstring("NUMERIC"));
      break;
    case GRB_SUBOPTIMAL:
      setlistelem(elem, "status", scalarstring("SUBOPTIMAL"));
      break;
    case GRB_INPROGRESS:
      setlistelem(elem, "status", scalarstring("INPROGRESS"));
      break;
    case GRB_USER_OBJ_LIMIT:
      setlistelem(elem, "status", scalarstring("USER_OBJ_LIMIT"));
      break;
    default:
      setlistelem(elem, "status", scalarstring("UNKNOWN"));
      break;
  }

  /* Ignore errors for these */

  GRBgetdblattr(model, GRB_DBL_ATTR_RUNTIME, &runtime);
  GRBgetdblattr(model, GRB_DBL_ATTR_ITERCOUNT, &itercount);
  GRBgetintattr(model, GRB_INT_ATTR_BARITERCOUNT, &baritercount);
  GRBgetdblattr(model, GRB_DBL_ATTR_NODECOUNT, &nodecount);

  setlistelem(elem, "runtime", Rf_ScalarReal(runtime));
  setlistelem(elem, "itercount", Rf_ScalarReal(itercount));
  setlistelem(elem, "baritercount", Rf_ScalarInteger(baritercount));
  setlistelem(elem, "nodecount", Rf_ScalarReal(nodecount));

  return 0;
}

static int getobjval(SEXP list, GRBmodel *model, int nmobjs, int pool)
{
  double *objval = NULL;
  int     n;
  int     error  = 0;

  if (nmobjs > 0) {
    objval = todbl(setlistelem(list, "objval", allocdblvector(nmobjs)));

    for (n = 0; n < nmobjs; n++) {
      error = GRBsetintparam(GRBgetenv(model), GRB_INT_PAR_OBJNUMBER, n);
      if (error) return error;
      error = GRBgetdblattr(model, GRB_DBL_ATTR_OBJNVAL, &objval[n]);
      if (error) return error;
    }
  } else {
    objval = todbl(setlistelem(list, "objval", Rf_ScalarReal(0.0)));

    if (pool)
      error = GRBgetdblattr(model, GRB_DBL_ATTR_POOLOBJVAL, objval);
    else
      error = GRBgetdblattr(model, GRB_DBL_ATTR_OBJVAL, objval);
    if (error) return error;
  }

  return 0;
}

static SEXP allocpoolsollist()
{
  static const
  char *NAMES[] = { "objval", "xn" };
  static const
  int   LEN     = ARRAY_LENGTH(NAMES);

  return alloclist(LEN, NAMES);
}

static int getprimsol(SEXP list, GRBmodel *model, const resultsizes *sizes)
{
  static const
  char *NAMES[] = { "objval", "x", "pool", "poolobjbound",
                    "slack", "qcslack" };
  static const
  int   LEN     = ARRAY_LENGTH(NAMES);

  int     nvars     = sizes->nvars;
  int     nconstrs  = sizes->nconstrs;
  int     nqconstrs = sizes->nqconstrs;
  int     nmobjs    = sizes->nmobjs;
  int     nsols     = sizes->nsols;
  SEXP    elem      = setlistelem(list, "getprimsol", alloclist(LEN, NAMES));
  double *x         = NULL;
  SEXP    poollist  = R_NilValue;
  SEXP    poolelem  = R_NilValue;
  double *xn        = NULL;
  double  poolbound = -GRB_INFINITY;
  double *slack     = NULL;
  double *qcslack   = NULL;
  int     ismip, n;
  int     error     = 0;

  error = GRBgetintattr(model, GRB_INT_ATTR_IS_MIP, &ismip);
  if (error) return error;

  error = getobjval(elem, model, nmobjs, 0);
  if (error) return error;

  if (nvars > 0) {
    x = todbl(setlistelem(elem, "x", allocdblvector(nvars)));

    error = GRBgetdblattrarray(model, GRB_DBL_ATTR_X, 0, nvars, x);
    if (error) return error;

    if (ismip || nmobjs > 0) {
      poollist = setlistelem(elem, "pool", alloclist(nsols, NULL));

      for (n = 0; n < nsols; n++) {
        SET_VECTOR_ELT(poollist, n, poolelem = allocpoolsollist());

        error = GRBsetintparam(GRBgetenv(model),
                               GRB_INT_PAR_SOLUTIONNUMBER, n);
        if (error) return error;

        error = getobjval(poolelem, model, nmobjs, 1);
        if (error) return error;

        xn = todbl(setlistelem(poolelem, "xn", allocdblvector(nvars)));

        error = GRBgetdblattrarray(model, GRB_DBL_ATTR_XN, 0, nvars, xn);
        if (error) return error;
      }
    }
  }

  if (ismip && nmobjs == 0) {

    /* ignore errors for PoolObjBound */

    GRBgetdblattr(model, GRB_DBL_ATTR_POOLOBJBOUND, &poolbound);

    if (fabs(poolbound) < GRB_INFINITY)
      setlistelem(elem, "poolobjbound", Rf_ScalarReal(poolbound));
  }

  if (nconstrs > 0) {
    slack = todbl(setlistelem(elem, "slack", allocdblvector(nconstrs)));

    error = GRBgetdblattrarray(model, GRB_DBL_ATTR_SLACK, 0, nconstrs, slack);
    if (error) return error;
  }

  if (nqconstrs > 0) {
    qcslack = todbl(setlistelem(elem, "qcslack", allocdblvector(nqconstrs)));

    error = GRBgetdblattrarray(model, GRB_DBL_ATTR_QCSLACK,
                               0, nqconstrs, qcslack);
    if (error) return error;
  }

  return 0;
}

static int getdualsol(SEXP list, GRBmodel *model, const resultsizes *sizes)
{
  static const
  char *NAMES[] = { "rc", "pi", "qcpi" };
  static const
  int   LEN     = ARRAY_LENGTH(NAMES);

  int     nvars     = sizes->nvars;
  int     nconstrs  = sizes->nconstrs;
  int     nqconstrs = sizes->nqconstrs;
  SEXP    elem      = setlistelem(list, "getdualsol", alloclist(LEN, NAMES));
  double *rc        = NULL;
  double *pi        = NULL;
  double *qcpi      = NULL;
  double  first;
  int     error     = 0;

  if (nvars > 0                                                   &&
     GRBgetdblattrelement(model, GRB_DBL_ATTR_RC, 0, &first) == 0   ) {
    rc = todbl(setlistelem(elem, "rc", allocdblvector(nvars)));

    error = GRBgetdblattrarray(model, GRB_DBL_ATTR_RC, 0, nvars, rc);
    if (error) return error;
  }

  if (nconstrs > 0                                                 &&
      GRBgetdblattrelement(model, GRB_DBL_ATTR_PI, 0, &first) == 0   ) {
    pi = todbl(setlistelem(elem, "pi", allocdblvector(nconstrs)));

    error = GRBgetdblattrarray(model, GRB_DBL_ATTR_PI, 0, nconstrs, pi);
    if (error) return error;
  }

  if (nqconstrs > 0                                                  &&
      GRBgetdblattrelement(model, GRB_DBL_ATTR_QCPI, 0, &first) == 0   ) {
    qcpi = todbl(setlistelem(elem, "qcpi", allocdblvector(nqconstrs)));

    error = GRBgetdblattrarray(model, GRB_DBL_ATTR_QCPI, 0, nqconstrs, qcpi);
    if (error) return error;
  }

  return 0;
}

static int getbasis(SEXP list, GRBmodel *model, const resultsizes *sizes)
{
  static const
  char *NAMES[] = { "vbasis", "cbasis" };
  static const
  int   LEN     = ARRAY_LENGTH(NAMES);

  int   nvars    = sizes->nvars;
  int   nconstrs = sizes->nconstrs;
  SEXP  elem     = setlistelem(list, "getbasis", alloclist(LEN, NAMES));
  int  *vbasis   = NULL;
  int  *cbasis   = NULL;
  int   first;
  int   error    = 0;

  if (nvars > 0                                                        &&
      GRBgetintattrelement(model, GRB_INT_ATTR_VBASIS, 0, &first) == 0   ) {
    vbasis = toint(setlistelem(elem, "vbasis", allocintvector(nvars)));

    error = GRBgetintattrarray(model, GRB_INT_ATTR_VBASIS, 0, nvars, vbasis);
    if (error) return error;
  }

  if (nconstrs > 0                                                     &&
      GRBgetintattrelement(model, GRB_INT_ATTR_CBASIS, 0, &first) == 0   ) {
    cbasis = toint(setlistelem(elem, "cbasis", allocintvector(nconstrs)));

    error = GRBgetintattrarray(model, GRB_INT_ATTR_CBASIS,
                               0, nconstrs, cbasis);
    if (error) return error;
  }

  return 0;
}

static int getinfunbdinfo(SEXP list, GRBmodel *model, const resultsizes *sizes)
{
  static const
  char *NAMES[] = { "unbdray", "farkasdual", "farkasproof" };
  static const
  int   LEN     = ARRAY_LENGTH(NAMES);

  int     nvars       = sizes->nvars;
  int     nconstrs    = sizes->nconstrs;
  SEXP    elem        = setlistelem(list, "getinfunbdinfo",
                                    alloclist(LEN, NAMES));
  double *unbdray     = NULL;
  double *farkasdual  = NULL;
  double  farkasproof;
  double  first;
  int     error       = 0;

  if (nvars > 0                                                         &&
      GRBgetdblattrelement(model, GRB_DBL_ATTR_UNBDRAY, 0, &first) == 0   ) {
    unbdray = todbl(setlistelem(elem, "unbdray", allocdblvector(nvars)));

    error = GRBgetdblattrarray(model, GRB_DBL_ATTR_UNBDRAY, 0, nvars, unbdray);
    if (error) return error;
  }

  if (nconstrs > 0                                                         &&
     GRBgetdblattrelement(model, GRB_DBL_ATTR_FARKASDUAL, 0, &first) == 0   ) {
    farkasdual = todbl(setlistelem(elem, "farkasdual",
                                   allocdblvector(nconstrs)));

    error = GRBgetdblattrarray(model, GRB_DBL_ATTR_FARKASDUAL,
                               0, nconstrs, farkasdual);
    if (error) return error;

    error = GRBgetdblattr(model, GRB_DBL_ATTR_FARKASPROOF, &farkasproof);
    if (error) return error;

    setlistelem(elem, "farkasproof", Rf_ScalarReal(farkasproof));
  }

  return 0;
}

static int getobjbounds(SEXP list, GRBmodel *model, const resultsizes *sizes)
{
  static const
  char *NAMES[] = { "objbound", "objboundc", "mipgap" };
  static const
  int   LEN     = ARRAY_LENGTH(NAMES);

  int    nmobjs    = sizes->nmobjs;
  SEXP   elem      = setlistelem(list, "getobjbounds", alloclist(LEN, NAMES));
  double objbound  = -GRB_INFINITY;
  double objboundc = -GRB_INFINITY;
  double mipgap    = -GRB_INFINITY;

  /* Ignore errors for these */

  if (nmobjs == 0) {
    GRBgetdblattr(model, GRB_DBL_ATTR_OBJBOUND, &objbound);
    GRBgetdblattr(model, GRB_DBL_ATTR_OBJBOUNDC, &objboundc);
    GRBgetdblattr(model, GRB_DBL_ATTR_MIPGAP, &mipgap);
  }

  if (fabs(objbound) < GRB_INFINITY)
    setlistelem(elem, "objbound", Rf_ScalarReal(objbound));
  if (fabs(objboundc) < GRB_INFINITY)
    setlistelem(elem, "objboundc", Rf_ScalarReal(objboundc));
  if (fabs(mipgap) < GRB_INFINITY)
    setlistelem(elem, "mipgap", Rf_ScalarReal(mipgap));

  return 0;
}

static int queryresults(SEXP *listP, GRBmodel *model)
{
  static const
  char *NAMES[] = { "getoptstats", "getprimsol", "getdualsol",
                    "getbasis", "getinfunbdinfo", "getobjbounds" };
  static const
  int   LEN     = ARRAY_LENGTH(NAMES);

  resultsizes sizes;
  int         error = 0;

  *listP = PROTECT(alloclist(LEN, NAMES));

  /* Get result sizes */

  error = getresultsizes(&sizes, model);
  if (error) goto QUIT;

  /* Get optimization statistics */

  error = getoptstats(*listP, model);
  if (error) goto QUIT;

  /* Get solution(s) */

  if (sizes.nsols > 0) {
    error = getprimsol(*listP, model, &sizes);
    if (error) goto QUIT;

    error = getdualsol(*listP, model, &sizes);
    if (error) goto QUIT;

    error = getbasis(*listP, model, &sizes);
    if (error) goto QUIT;
  }

  /* Get infeasible or unbounded info */

  error = getinfunbdinfo(*listP, model, &sizes);
  if (error) goto QUIT;

  /* Get objective bounds */

  error = getobjbounds(*listP, model, &sizes);
  if (error) goto QUIT;

QUIT:

  *listP = mergesublists(*listP);

  UNPROTECT(1);

  return error;
}

/* Querying IIS ------------------------------------------------------------ */

typedef struct _iissizes
{
  int nvars;
  int nconstrs;
  int nqconstrs;
  int nsos;
  int ngconstrs;
} iissizes;

static int getiissizes(iissizes *sizes, GRBmodel *model)
{
  int error = 0;

  error = GRBgetintattr(model, GRB_INT_ATTR_NUMVARS, &(sizes->nvars));
  if (error) return error;
  error = GRBgetintattr(model, GRB_INT_ATTR_NUMCONSTRS, &(sizes->nconstrs));
  if (error) return error;
  error = GRBgetintattr(model, GRB_INT_ATTR_NUMQCONSTRS, &(sizes->nqconstrs));
  if (error) return error;
  error = GRBgetintattr(model, GRB_INT_ATTR_NUMSOS, &(sizes->nsos));
  if (error) return error;
  error = GRBgetintattr(model, GRB_INT_ATTR_NUMGENCONSTRS,
                        &(sizes->ngconstrs));

  return 0;
}

static int getiisstats(SEXP list, GRBmodel *model)
{
  SEXP   elem    = setlistelem(list, "getiisstats", alloclen1list("minimal"));
  int    minimal;
  int    error   = 0;

  error = GRBgetintattr(model, GRB_INT_ATTR_IIS_MINIMAL, &minimal);
  if (error) return error;

  setlistelem(elem, "minimal", Rf_ScalarInteger(minimal));

  return 0;
}

static int getiisbasic(SEXP list, GRBmodel *model, const iissizes *sizes)
{
  static const
  char *NAMES[] = { "Arows", "lb", "ub", "quadcon", "sos" };
  static const
  int   LEN     = ARRAY_LENGTH(NAMES);

  int   nconstrs  = sizes->nconstrs;
  int   nvars     = sizes->nvars;
  int   nqconstrs = sizes->nqconstrs;
  int   nsos      = sizes->nsos;
  SEXP  elem      = setlistelem(list, "getiisbasic", alloclist(LEN, NAMES));
  int  *constrs   = NULL;
  int  *lb        = NULL;
  int  *ub        = NULL;
  int  *qconstrs  = NULL;
  int  *sos       = NULL;
  int   error     = 0;

  if (nconstrs > 0) {
    constrs = toint(setlistelem(elem, "Arows", allocintvector(nconstrs)));

    error = GRBgetintattrarray(model, GRB_INT_ATTR_IIS_CONSTR,
                               0, nconstrs, constrs);
    if (error) return error;
  }

  if (nvars > 0) {
    lb = toint(setlistelem(elem, "lb", allocintvector(nvars)));
    ub = toint(setlistelem(elem, "ub", allocintvector(nvars)));

    error = GRBgetintattrarray(model, GRB_INT_ATTR_IIS_LB, 0, nvars, lb);
    if (error) return error;
    error = GRBgetintattrarray(model, GRB_INT_ATTR_IIS_UB, 0, nvars, ub);
    if (error) return error;
  }

  if (nqconstrs > 0) {
    qconstrs = toint(setlistelem(elem, "quadcon", allocintvector(nqconstrs)));

    error = GRBgetintattrarray(model, GRB_INT_ATTR_IIS_QCONSTR,
                               0, nqconstrs, qconstrs);
    if (error) return error;
  }

  if (nsos > 0) {
    sos = toint(setlistelem(elem, "sos", allocintvector(nsos)));

    error = GRBgetintattrarray(model, GRB_INT_ATTR_IIS_SOS, 0, nsos, sos);
    if (error) return error;
  }

  return 0;
}

static int getiisgeneral(SEXP list, GRBmodel *model, const iissizes *sizes)
{
  /* the order of the general constraint names need to match the order of
   * the general constraint defines in api/public.h
   */
  static const
  char *NAMES[] = { "genconmax", "genconmin", "genconabs",
                    "genconand", "genconor", "genconind", "genconpwl",
                    "genconpoly","genconexp", "genconexpa", "genconlog", "genconloga",
                    "genconpow", "genconsin", "genconcos", "gencontan" };
  static const
  int   LEN     = ARRAY_LENGTH(NAMES);

  int   ngconstrs      = sizes->ngconstrs;
  SEXP  elem           = setlistelem(list, "getiisgeneral",
                                     alloclist(LEN, NAMES));
  int   ngctype[LEN];
  int  *gctypes        = NULL;
  int  *gciis          = NULL;
  int  *gctypeiis[LEN];
  int   gc, t;
  int   error          = 0;

  if (ngconstrs == 0) goto QUIT;

  memset(ngctype, 0, LEN * sizeof(int));

  gctypes = (int *) malloc(ngconstrs * sizeof(int));
  gciis = (int *) malloc(ngconstrs * sizeof(int));
  if (gctypes == NULL || gciis == NULL) {
    error = ERROR_OUT_OF_MEMORY;
    goto QUIT;
  }

  error = GRBgetintattrarray(model, GRB_INT_ATTR_GENCONSTRTYPE,
                             0, ngconstrs, gctypes);
  if (error) goto QUIT;
  error = GRBgetintattrarray(model, GRB_INT_ATTR_IIS_GENCONSTR,
                             0, ngconstrs, gciis);
  if (error) goto QUIT;

  for (gc = 0; gc < ngconstrs; gc++) {
    t = gctypes[gc];
    if (t < LEN) {
      ngctype[t]++;
    } else {
      error = ERROR_UNKNOWN_GENCONSTR;
      goto QUIT;
    }
  }

  for (t = 0; t < LEN; t++)
    if (ngctype[t] > 0)
      gctypeiis[t] = toint(setlistelem(elem, NAMES[t],
                                       allocintvector(ngctype[t])));

  memset(ngctype, 0, LEN * sizeof(int));

  for (gc = 0; gc < ngconstrs; gc++) {
    t = gctypes[gc];
    gctypeiis[t][ngctype[t]++] = gciis[gc];
  }

QUIT:

  free(gctypes);
  free(gciis);

  return error;
}

static int queryiis(SEXP *listP, GRBmodel *model)
{
  static const
  char *NAMES[] = { "getiisstats", "getiisbasic", "getiisgeneral" };
  static const
  int   LEN     = ARRAY_LENGTH(NAMES);

  iissizes sizes;
  int      error = 0;

  *listP = PROTECT(alloclist(LEN, NAMES));

  /* Get IIS sizes */

  error = getiissizes(&sizes, model);
  if (error) goto QUIT;

  /* Get IIS statistics */

  error = getiisstats(*listP, model);
  if (error) goto QUIT;

  /* Get IIS for basic contraints and bounds */

  error = getiisbasic(*listP, model, &sizes);
  if (error) goto QUIT;

  /* Get IIS for general contraints */

  error = getiisgeneral(*listP, model, &sizes);
  if (error) goto QUIT;

QUIT:

  *listP = mergesublists(*listP);

  UNPROTECT(1);

  return error;
}

/* Querying FeasRelax ------------------------------------------------------ */

static int queryfeasrelax(SEXP *listP, GRBmodel *model, SEXP args)
{
  static const
  char *NAMES[] = { "model", "feasobj" };
  static const
  int   LEN     = ARRAY_LENGTH(NAMES);

  int     relaxobjtype = getlistint(args, "relaxobjtype", 0);
  int     minrelax     = getlistint(args, "minrelax", 0);
  double *lbpen        = todbl(getlistelem(args, "lbpen"));
  double *ubpen        = todbl(getlistelem(args, "ubpen"));
  double *rhspen       = todbl(getlistelem(args, "rhspen"));
  SEXP    feasmodel    = R_NilValue;
  double  feasobj      = 0.0;
  int     error        = 0;

  *listP = PROTECT(alloclist(LEN, NAMES));

  error = GRBfeasrelax(model, relaxobjtype, minrelax,
                       lbpen, ubpen, rhspen, &feasobj);
  if (error) goto QUIT;
  setlistelem(*listP, "feasobj", Rf_ScalarReal(feasobj));
  error = querymodel(&feasmodel, model);
  if (error) goto QUIT;
  setlistelem(*listP, "model", feasmodel);

QUIT:

  UNPROTECT(1);

  return error;
}

/* Actions for GRBmodel ---------------------------------------------------- */

static int takeaction(SEXP *listP, GRBenv *env, GRBmodel *model, SEXP args)
{
  SEXP        optimize_args      = getlistelem(args, "optimize");
  SEXP        readmodel_args     = getlistelem(args, "readmodel");
  SEXP        write_args         = getlistelem(args, "write");
  SEXP        computeIIS_args    = getlistelem(args, "computeIIS");
  SEXP        feasrelax_args     = getlistelem(args, "feasrelax");
  SEXP        relaxmodel_args    = getlistelem(args, "relaxmodel");
  SEXP        presolvemodel_args = getlistelem(args, "presolvemodel");
  GRBmodel   *outputmodel        = NULL;
  const char *filename;
  int         error          = 0;

  if (!Rf_isNull(optimize_args)) {

    error = GRBoptimize(model);
    if (error) goto QUIT;
    error = queryresults(listP, model);
    if (error) goto QUIT;

  } else if (!Rf_isNull(readmodel_args)) {

    filename = getlistchr(readmodel_args, "filename", "");
    error = GRBreadmodel(env, filename, &outputmodel);
    if (error) goto QUIT;
    error = querymodel(listP, outputmodel);
    if (error) goto QUIT;

  } else if (!Rf_isNull(write_args)) {

    filename = getlistchr(write_args, "filename", "");
    error = GRBwrite(model, filename);
    if (error) goto QUIT;

  } else if (!Rf_isNull(computeIIS_args)) {

    error = GRBcomputeIIS(model);
    if (error) goto QUIT;
    error = queryiis(listP, model);
    if (error) goto QUIT;

  } else if (!Rf_isNull(feasrelax_args)) {

    error = queryfeasrelax(listP, model, feasrelax_args);
    if (error) goto QUIT;

  } else if (!Rf_isNull(relaxmodel_args)) {

    error = GRBrelaxmodel(model, &outputmodel);
    if (error) {
      goto QUIT;
    } else if (outputmodel == NULL) {
      error = ERROR_RELAX_MODEL;
      goto QUIT;
    }
    error = querymodel(listP, outputmodel);
    if (error) goto QUIT;

  } else if (!Rf_isNull(presolvemodel_args)) {

    error = GRBpresolvemodel(model, &outputmodel);
    if (error) {
      goto QUIT;
    } else if (outputmodel == NULL) {
      error = ERROR_PRESOLVE_MODEL;
      goto QUIT;
    }
    error = querymodel(listP, outputmodel);
    if (error) goto QUIT;

  }

QUIT:

  GRBfreemodel(outputmodel);

  return error;
}

/* Called from R ----------------------------------------------------------- */

SEXP decompress_sparse_pointers(SEXP p)
{
  int   n      = Rf_length(p) - 1;
  int  *beg    = toint(p);
  int   nnzs   = beg[n];
  SEXP  vector = PROTECT(allocintvector(nnzs));
  int  *ind    = toint(vector);
  int   j, nz;

  for (j = 0; j < n; j++)
    for (nz = beg[j]; nz < beg[j + 1]; nz++)
      ind[nz] = j;

  UNPROTECT(1);

  return vector;
}

SEXP are_triplets_sorted(SEXP i, SEXP j)
{
  int  nnzs = Rf_length(i);
  int *row  = toint(i);
  int *col  = toint(j);

  /* Check if sorted by row and then (strict) col order */

  switch (lexicographicordering(row, col, nnzs)) {
    case ORDER_BOTH_STRICT:
      return scalarstring("byboth");
    case ORDER_FIRST_ONLY:
      return scalarstring("byrow");
  }

  /* Check if sorted by col and then (strict) row order */

  switch (lexicographicordering(col, row, nnzs)) {
    case ORDER_BOTH_STRICT:
      return scalarstring("byboth");
    case ORDER_FIRST_ONLY:
      return scalarstring("bycol");
  }

  return scalarstring("no");
}

SEXP aggregate_triplets(SEXP sorted, SEXP i, SEXP j, SEXP v, SEXP dims)
{
  int     nrows  = toint(dims)[0];
  int     ncols  = toint(dims)[1];
  int     nnzs   = Rf_length(i);
  SEXP    vector = PROTECT(Rf_duplicate(v));
  double *val    = todbl(vector);
  int    *row    = toint(i);
  int    *col    = toint(j);

  if (nnzs < 2) goto QUIT;

  if (strcmp(CHAR(STRING_ELT(sorted, 0)), "byrow") == 0)
    aggregateorderedvalues(val, row, col, ncols, nnzs);
  else if (strcmp(CHAR(STRING_ELT(sorted, 0)), "bycol") == 0)
    aggregateorderedvalues(val, col, row, nrows, nnzs);
  else if (nrows <= ncols)
    aggregatevalues(val, row, col, nrows, ncols, nnzs);
  else /* if we transpose, then sort is O(nnzs + min(dims)) */
    aggregatevalues(val, col, row, ncols, nrows, nnzs);

QUIT:

  UNPROTECT(1);

  return vector;
}

SEXP gurobi_common(SEXP env_args, SEXP model_args, SEXP action_args)
{
  GRBenv   *env      = NULL;
  GRBmodel *model    = NULL;
  SEXP      list     = R_NilValue;
  int       display  = 0;
  int       error    = 0;
  char     *errormsg = NULL;

  error = buildenv(&env, &display, env_args);
  if (error) goto QUIT;

  if (Rf_length(model_args) > 0) {
    error = buildmodel(env, &model, display, model_args);
    if (error) goto QUIT;
  }

  error = takeaction(&list, env, model, action_args);
  if (error) goto QUIT;

QUIT:

  if (error > 0) {
    errormsg = (char *) R_alloc(strlen(GRBgeterrormsg(env)) + 1, sizeof(char));
    strcpy(errormsg, GRBgeterrormsg(env));
  }

  GRBfreemodel(model);
  GRBfreeenv(env);

  if (error > 0)
    Rf_error("Error %d: %s\n", error, errormsg);
  else
    switch (error) {
      case ERROR_OUT_OF_MEMORY:
        Rf_error("Out of memory\n");
      case ERROR_UNKNOWN_GENCONSTR:
        Rf_error("General constraint of unknown type\n");
      case ERROR_RELAX_MODEL:
        Rf_error("Unable to relax model\n");
      case ERROR_PRESOLVE_MODEL:
        Rf_error("Unable to presolve model\n");
    }

  return list;
}
