/*
###############################################################################
#
#  EGSnrc egs++ functions
#  Copyright (C) 2015 National Research Council Canada
#
#  This file is part of EGSnrc.
#
#  EGSnrc is free software: you can redistribute it and/or modify it under
#  the terms of the GNU Affero General Public License as published by the
#  Free Software Foundation, either version 3 of the License, or (at your
#  option) any later version.
#
#  EGSnrc is distributed in the hope that it will be useful, but WITHOUT ANY
#  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
#  FOR A PARTICULAR PURPOSE.  See the GNU Affero General Public License for
#  more details.
#
#  You should have received a copy of the GNU Affero General Public License
#  along with EGSnrc. If not, see <http://www.gnu.org/licenses/>.
#
###############################################################################
#
#  Author:          Iwan Kawrakow, 2005
#
#  Contributors:
#
###############################################################################
*/


/*! \file egs_functions.cpp
 *  \brief Global egspp functions implementation
 *  \IK
 */

#include "egs_functions.h"
#include "egs_application.h"

#include <cstdio>
#include <cstdarg>
#include <cstdlib>
#include <cctype>

#ifdef WIN32
    const char __egs_fs = 92;
#else
    const char __egs_fs = '/';
#endif

/*
extern "C" void __attribute__((destructor)) finish_egspp_library() {
    egsInformation("In finish_egspp_library()\n");
}
*/

bool EGS_EXPORT egsStoreI64(ostream &data, EGS_I64 n) {
    EGS_I64 i1 = n;
    i1 /= 1000000000;
    EGS_I64 i2 = n % 1000000000;
    data << " " << (int) i1 << " " << (int) i2;
    if (data.fail()) {
        return false;
    }
    return true;
}

bool EGS_EXPORT egsGetI64(istream &data, EGS_I64 &n) {
    int i1, i2;
    data >> i1 >> i2;
    if (data.eof() || !data.good()) {
        return false;
    }
    n = i1;
    n *= 1000000000;
    n += i2;
    return true;
}

static FILE *egs_info_fp = stdout;
static FILE *egs_warning_fp = stderr;
static FILE *egs_error_fp = stderr;

static EGS_LOCAL char __egsf_write_buf[8192];

static void EGS_LOCAL __egs_default_information(const char *msg, ...) {
    va_list ap;
    va_start(ap, msg);
    EGS_Application *a = EGS_Application::activeApplication();
    if (a) {
        vsprintf(__egsf_write_buf,msg,ap);
        a->appInformation(__egsf_write_buf);
    }
    else {
        vfprintf(egs_info_fp, msg, ap);
        fflush(egs_info_fp);
    }
    va_end(ap);
}

static bool egs_b_hide_warning = true;

void egsWarningVisible(bool visible) { egs_b_hide_warning = !visible; }

static void EGS_LOCAL __egs_default_warning(const char *msg, ...) {
	if (egs_b_hide_warning) return;
    va_list ap;
    va_start(ap, msg);
    EGS_Application *a = EGS_Application::activeApplication();
    if (a) {
        vsprintf(__egsf_write_buf,msg,ap);
        a->appWarning(__egsf_write_buf);
    }
    else {

        vfprintf(egs_warning_fp, msg, ap);
        fflush(egs_warning_fp);
    }
    va_end(ap);
}

static void EGS_LOCAL __egs_default_error(const char *msg, ...) {
    va_list ap;
    va_start(ap, msg);
    EGS_Application *a = EGS_Application::activeApplication();
    if (a) {
        vsprintf(__egsf_write_buf,msg,ap);
        va_end(ap);
        a->appFatal(__egsf_write_buf);
    }
    else {
        vfprintf(egs_error_fp, msg, ap);
        fflush(stderr);
        va_end(ap);
        exit(1);
    }
}

EGS_InfoFunction EGS_EXPORT egsInformation = __egs_default_information;
EGS_InfoFunction EGS_EXPORT egsWarning = __egs_default_warning;
EGS_InfoFunction EGS_EXPORT egsFatal = __egs_default_error;

EGS_InfoFunction egsSetInfoFunction(EGS_InfoType t, EGS_InfoFunction func) {
    if (!func) {
        egsWarning("egsSetInfoFunction: info function can not be NULL!\n");
        return 0;
    }
    EGS_InfoFunction res;
    switch (t) {
    case Information:
        res = egsInformation;
        egsInformation = func;
        break;
    case Warning:
        res = egsWarning;
        egsWarning = func;
        break;
    case Fatal:
        res = egsFatal;
        egsFatal = func;
        break;
    default:
        egsWarning("Unknown info function type\n");
        res = 0;
    }
    return res;
}

void egsSetDefaultIOFunctions() {
    egsInformation = __egs_default_information;
    egsWarning = __egs_default_warning;
    egsFatal = __egs_default_error;
}

template <class T> void __egs_swap_bytes(T *v) {
    char *c = (char *) v;
    char tmp = c[0];
    c[0]=c[3];
    c[3]=tmp;
    tmp = c[1];
    c[1]=c[2];
    c[2]=tmp;
}

void egsSwapBytes(int *n) {
    __egs_swap_bytes<int>(n);
}
void egsSwapBytes(float *n) {
    __egs_swap_bytes<float>(n);
}

void egsSwapBytes(short *n) {
    char *c = (char *) n;
    char tmp=c[0];
    c[0]=c[1];
    c[1]=tmp;
}

string egsJoinPath(const string &first, const string &second) {
    int n = first.size()-1;
    char c = first[n];
    string result(first);
    if (c == '/' && c != __egs_fs) {
        result[n] = __egs_fs;
    }
    if (result[n] != __egs_fs) {
        result += __egs_fs;
    }
    result += second;
    return result;
}

string egsStripPath(const string &aname) {
    int j;
    for (j=aname.size()-1; j>=0; j--) {
        if (aname[j] == '/' || aname[j] == __egs_fs) {
            j++;
            break;
        }
    }
    if (j < 0) {
        return aname;
    }
    string result;
    if (j >= 0) {
        while (j < aname.size()) {
            result += aname[j++];
        }
    }
    return result;
}

#ifdef WIN32
    #include <process.h>
#else
    #include <sys/types.h>
    #include <unistd.h>
#endif

string egsHostName() {
#ifdef WIN32
    // Windows has the gethostname() function but it only works after
    // calling WSAStartup and in addition we have to link against the
    // ws2_32 DLL. This is pretty stupid as we just want to get the host name =>
    // we try to get the host name from environment variables.
    char *var = getenv("COMPUTERNAME");
    if (!var) {
        var = getenv("HOSTNAME");
        if (!var) {
            var = getenv("HOST");
        }
    }
    if (!var) {
        return "unknown";
    }
    return var;
#else
    char buf[1024];
    int err = gethostname(buf,1023);
    if (err) {
        return "unknown";
    }
    return buf;
#endif
}

int egsGetPid() {
#ifdef WIN32
    return _getpid();
#else
    return getpid();
#endif
}

string egsSimplifyCVSKey(const string &key) {
    if (key.size() < 2) {
        return key;
    }
    int js;
    for (js=0; js<key.size(); js++) if (key[js] == '$') {
            break;
        }
    if (js >= key.size()) {
        return key;
    }
    int je;
    for (je=key.size()-1; je>=0; je--) if (key[je] == '$') {
            break;
        }
    if (je <= js) {
        return key;
    }
    string result;
    for (int j=js+1; j<je; j++) {
        result += key[j];
    }
    return result;
}

int egsGetEndian() {
    int nl = 0x12345678;
    unsigned char *p = (unsigned char *)(&nl);
    if (p[0] == 0x12 && p[1] == 0x34 && p[2] == 0x56 && p[3] == 0x78) {
        return 0;
    }
    if (p[0] == 0x78 && p[1] == 0x56 && p[2] == 0x34 && p[3] == 0x12) {
        return 1;
    }
    return -1;
}

bool egsIsAbsolutePath(const string &path) {
#ifdef WIN32
    if (path.size() < 2) {
        return false;
    }
    if ((path[0] == '/' || path[0] == __egs_fs) &&
            (path[1] == '/' || path[1] == __egs_fs)) {
        return true;
    }
    if (path.size() < 3) {
        return false;
    }
    if (!isalpha(path[0])) {
        return false;
    }
    if (path[1] != ':') {
        return false;
    }
    if (path[2] != '/' && path[2] != __egs_fs) {
        return false;
    }
    return true;
#else
    return (path[0] == '/');
#endif
}

