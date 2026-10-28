#ifndef _WIN32
#define _GNU_SOURCE /* dladdr() on glibc */
#endif

#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include <zstd.h>

#ifdef _WIN32
#include <windows.h>
#else
#include <dlfcn.h>
#endif

SEXP zstd_version_(void);

/* Path of the shared library or executable that contains `addr`, or
 * NA if it cannot be determined. */
static SEXP module_path(const void* addr) {
#ifdef _WIN32
  HMODULE mod = NULL;
  if (!GetModuleHandleExW(GET_MODULE_HANDLE_EX_FLAG_FROM_ADDRESS |
                              GET_MODULE_HANDLE_EX_FLAG_UNCHANGED_REFCOUNT,
                          (LPCWSTR)addr, &mod)) {
    return NA_STRING;  // # nocov
  }
  wchar_t wpath[MAX_PATH];
  DWORD wlen = GetModuleFileNameW(mod, wpath, MAX_PATH);
  if (wlen == 0 || wlen == MAX_PATH) {
    return NA_STRING;  // # nocov
  }
  char path[MAX_PATH * 4];
  int len = WideCharToMultiByte(CP_UTF8, 0, wpath, (int)wlen, path,
                                (int)sizeof(path), NULL, NULL);
  if (len == 0) {
    return NA_STRING;  // # nocov
  }
  return Rf_mkCharLenCE(path, len, CE_UTF8);
#else
  Dl_info info;
  if (dladdr(addr, &info) == 0 || info.dli_fname == NULL) {
    return NA_STRING;  // # nocov
  }
  return Rf_mkCharCE(info.dli_fname, CE_NATIVE);
#endif
}

SEXP zstd_version_(void) {
  const char* names[] = {"type", "version", "path", "own_path", ""};
  SEXP res = PROTECT(Rf_mkNamed(VECSXP, names));
#ifdef ZSTD_R_BUNDLED
  SET_VECTOR_ELT(res, 0, Rf_mkString("bundled"));
#else
  SET_VECTOR_ELT(res, 0, Rf_mkString("system"));
#endif
  SET_VECTOR_ELT(res, 1, Rf_mkString(ZSTD_versionString()));
  SET_VECTOR_ELT(
      res, 2, Rf_ScalarString(module_path((const void*)&ZSTD_versionString)));
  SET_VECTOR_ELT(res, 3,
                 Rf_ScalarString(module_path((const void*)&zstd_version_)));
  UNPROTECT(1);
  return res;
}
