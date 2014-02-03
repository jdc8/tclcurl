package require critcl 3

namespace eval ::curl {
}

critcl::userconfig define mode {choose mode of Curl to build and link against.} {static dynamic}

if {[string match "win32*" [::critcl::targetplatform]]} {
    critcl::clibraries -llibcurl_a
    switch -exact -- [critcl::userconfig query mode] {
	static {
	    critcl::cflags /DDLL_EXPORT
	}
	dynamic {
	}
    }
} else {
    switch -exact -- [critcl::userconfig query mode] {
	static {
	    critcl::clibraries -l:libcurl.a -l:libssl.a -l:libcrypto.a
	}
	dynamic {
	    critcl::clibraries -lcurl -lssl -lcrypto
	}
    }

    critcl::clibraries -lpthread -lm

    if {[string match "macosx*" [::critcl::targetplatform]]} {
	critcl::clibraries -lgcc_eh
    } else {
	critcl::clibraries -lrt -luuid
    }
}

# Get local build configuration
if {[file exists "[file dirname [info script]]/tclcurl_config.tcl"]} {
    set fd [open "[file dirname [info script]]/tclcurl_config.tcl"]
    eval [read $fd]
    close $fd
}

critcl::tcl 8.6
critcl::csources multi.c tclcurl.c
critcl::tsources tclcurl_tcl.tcl

critcl::ccode {

#include "tclcurl.h"
#include "multi.h"

#include <sys/types.h>
#ifdef _WIN32
#else
#include <unistd.h>
#endif

}

#*
#*----------------------------------------------------------------------
#*
#* curlInitObjCmd --
#*
#*  This procedure is invoked to process the "curl::init" Tcl command.
#*  See the user documentation for details on what it does.
#*
#* Results:
#*  A standard Tcl result.
#*
#* Side effects:
#*  See the user documentation.
#*
#*----------------------------------------------------------------------
#*/
critcl::ccommand ::curl::init {cd interp objc objv} {

    Tcl_Obj             *resultObjPtr;
    CURL                *curlHandle;
    struct curlObjData  *curlData;
    char                *handleName;

    if (objc != 1) {
	Tcl_WrongNumArgs(interp, 1, objv, "");
        return TCL_ERROR;
    }

    curlData=(struct curlObjData *)Tcl_Alloc(sizeof(struct curlObjData));
    if (curlData==NULL) {
        resultObjPtr=Tcl_NewStringObj("Couldn't allocate memory",-1);
        Tcl_SetObjResult(interp,resultObjPtr);
        return TCL_ERROR;
    }

    memset(curlData, 0, sizeof(struct curlObjData));
    curlData->interp=interp;

    curlHandle=curl_easy_init();
    if (curlHandle==NULL) {
        resultObjPtr=Tcl_NewStringObj("Couldn't open curl handle",-1);
        Tcl_SetObjResult(interp,resultObjPtr);
        return TCL_ERROR;
    }

    handleName=curlCreateObjCmd(interp,curlData);

    curlData->curl=curlHandle;

    resultObjPtr=Tcl_NewStringObj(handleName,-1);
    Tcl_SetObjResult(interp,resultObjPtr);
    Tcl_Free(handleName);

    return TCL_OK;
}

#*
#*----------------------------------------------------------------------
#*
#* curlVersion --
#*
#*	This procedure is invoked to process the "curl::init" Tcl command.
#*	See the user documentation for details on what it does.
#*
#* Parameters:
#*  The stantard parameters for Tcl commands
#*
#* Results:
#*	A standard Tcl result.
#*
#* Side effects:
#*	See the user documentation.
#*
#*----------------------------------------------------------------------
#*/
critcl::ccommand ::curl::version {cd interp objc objv} {

    Tcl_Obj     *versionPtr;
    char        tclversion[200];

    if (objc != 1) {
	Tcl_WrongNumArgs(interp, 1, objv, "");
        return TCL_ERROR;
    }

    sprintf(tclversion,"TclCurl Version %s (%s)",TclCurlVersion,
                                                 curl_version());
    versionPtr=Tcl_NewStringObj(tclversion,-1);
    Tcl_SetObjResult(interp,versionPtr);

    return TCL_OK;
}

#*
#*----------------------------------------------------------------------
#*
#* curlEscape --
#*
#*  This function is invoked to process the "curl::escape" Tcl command.
#*  See the user documentation for details on what it does.
#*
#*
#* Parameters:
#*  The stantard parameters for Tcl commands
#*
#* Results:
#*  A standard Tcl result.
#*
#* Side effects:
#*  See the user documentation.
#*
#*----------------------------------------------------------------------
#*/
critcl::ccommand ::curl::escape {cd interp objc objv} {

    Tcl_Obj        *resultObj;
    char           *escapedStr;

    if (objc != 2) {
	Tcl_WrongNumArgs(interp, 1, objv, "url");
        return TCL_ERROR;
    }

    escapedStr=curl_easy_escape(NULL,Tcl_GetString(objv[1]),0);

    if(!escapedStr) {
        resultObj=Tcl_NewStringObj("curl::escape bad parameter",-1);
        Tcl_SetObjResult(interp,resultObj);
        return TCL_ERROR;
    }
    resultObj=Tcl_NewStringObj(escapedStr,-1);
    Tcl_SetObjResult(interp,resultObj);
    curl_free(escapedStr);

    return TCL_OK;
}

#*
#*----------------------------------------------------------------------
#*
#* curlUnescape --
#*
#*  This function is invoked to process the "curl::Unescape" Tcl command.
#*  See the user documentation for details on what it does.
#*
#*
#* Parameters:
#*  The stantard parameters for Tcl commands
#*
#* Results:
#*  A standard Tcl result.
#*
#* Side effects:
#*  See the user documentation.
#*
#*----------------------------------------------------------------------
#*/
critcl::ccommand ::curl::unescape {cd interp objc objv} {

    Tcl_Obj        *resultObj;
    char           *unescapedStr;

    if (objc != 2) {
	Tcl_WrongNumArgs(interp, 1, objv, "url");
        return TCL_ERROR;
    }

    unescapedStr=curl_easy_unescape(NULL,Tcl_GetString(objv[1]),0,NULL);
    if(!unescapedStr) {
        resultObj=Tcl_NewStringObj("curl::unescape bad parameter",-1);
        Tcl_SetObjResult(interp,resultObj);
        return TCL_ERROR;
    }
    resultObj=Tcl_NewStringObj(unescapedStr,-1);
    Tcl_SetObjResult(interp,resultObj);
    curl_free(unescapedStr);

    return TCL_OK;
}

#*
#*----------------------------------------------------------------------
#*
#* curlVersionInfo --
#*
#*  This function invokes 'curl_version_info' to query how 'libcurl' was
#*  compiled.
#*
#* Parameters:
#*  The standard parameters for Tcl commands, but nothing is used.
#*
#* Results:
#*  A standard Tcl result.
#*
#* Side effects:
#*  See the user documentation.
#*
#*----------------------------------------------------------------------
#*/
critcl::ccommand ::curl::versioninfo {cd interp objc objv} {

    int                            tableIndex;
    int                            i;
    curl_version_info_data        *infoPtr;
    Tcl_Obj                       *resultObjPtr=NULL;
    char                           tmp[7];

    if (objc != 2) {
	Tcl_WrongNumArgs(interp, 1, objv, "-option");
        return TCL_ERROR;
    }

    if (Tcl_GetIndexFromObj(interp, objv[1], versionInfoTable, "option",
            TCL_EXACT,&tableIndex)==TCL_ERROR) {
        return TCL_ERROR;
    }

    infoPtr=curl_version_info(CURLVERSION_NOW);

    switch(tableIndex) {
        case 0:
            resultObjPtr=Tcl_NewStringObj(infoPtr->version,-1);
            break;
        case 1:
            sprintf(tmp,"%X",infoPtr->version_num);
            resultObjPtr=Tcl_NewStringObj(tmp,-1);
            break;
        case 2:
            resultObjPtr=Tcl_NewStringObj(infoPtr->host,-1);
            break;
        case 3:
            resultObjPtr=Tcl_NewListObj(0,(Tcl_Obj **)NULL);
            if (infoPtr->features&CURL_VERSION_IPV6) {
                Tcl_ListObjAppendElement(interp,resultObjPtr
                        ,Tcl_NewStringObj("IPV6",-1));
            }
            if (infoPtr->features&CURL_VERSION_KERBEROS4) {
                Tcl_ListObjAppendElement(interp,resultObjPtr
                        ,Tcl_NewStringObj("KERBEROS4",-1));
            }
            if (infoPtr->features&CURL_VERSION_SSL) {
                Tcl_ListObjAppendElement(interp,resultObjPtr
                        ,Tcl_NewStringObj("SSL",-1));
            }
            if (infoPtr->features&CURL_VERSION_LIBZ) {
                Tcl_ListObjAppendElement(interp,resultObjPtr
                        ,Tcl_NewStringObj("LIBZ",-1));
            }
            if (infoPtr->features&CURL_VERSION_NTLM) {
                Tcl_ListObjAppendElement(interp,resultObjPtr
                        ,Tcl_NewStringObj("NTLM",-1));
            }
            if (infoPtr->features&CURL_VERSION_GSSNEGOTIATE) {
                Tcl_ListObjAppendElement(interp,resultObjPtr
                        ,Tcl_NewStringObj("GSSNEGOTIATE",-1));
            }
            if (infoPtr->features&CURL_VERSION_DEBUG) {
                Tcl_ListObjAppendElement(interp,resultObjPtr
                        ,Tcl_NewStringObj("DEBUG",-1));
            }
            if (infoPtr->features&CURL_VERSION_ASYNCHDNS) {
                Tcl_ListObjAppendElement(interp,resultObjPtr
                        ,Tcl_NewStringObj("ASYNCHDNS",-1));
            }
            if (infoPtr->features&CURL_VERSION_SPNEGO) {
                Tcl_ListObjAppendElement(interp,resultObjPtr
                        ,Tcl_NewStringObj("SPNEGO",-1));
            }
            if (infoPtr->features&CURL_VERSION_LARGEFILE) {
                Tcl_ListObjAppendElement(interp,resultObjPtr
                        ,Tcl_NewStringObj("LARGEFILE",-1));
            }
            if (infoPtr->features&CURL_VERSION_IDN) {
                Tcl_ListObjAppendElement(interp,resultObjPtr
                        ,Tcl_NewStringObj("IDN",-1));
            }
            if (infoPtr->features&CURL_VERSION_SSPI) {
                Tcl_ListObjAppendElement(interp,resultObjPtr
                        ,Tcl_NewStringObj("SSPI",-1));
            }
            if (infoPtr->features&CURL_VERSION_CONV) {
                Tcl_ListObjAppendElement(interp,resultObjPtr
                        ,Tcl_NewStringObj("CONV",-1));
            }
            if (infoPtr->features&CURL_VERSION_CURLDEBUG) {
                Tcl_ListObjAppendElement(interp,resultObjPtr
                        ,Tcl_NewStringObj("CURLDEBUG",-1));
            }
            if (infoPtr->features&CURL_VERSION_TLSAUTH_SRP) {
                Tcl_ListObjAppendElement(interp,resultObjPtr
                        ,Tcl_NewStringObj("TLSAUTH_SRP",-1));
            }
            if (infoPtr->features&CURL_VERSION_NTLM_WB) {
                Tcl_ListObjAppendElement(interp,resultObjPtr
                        ,Tcl_NewStringObj("NTLM_WB",-1));
            }
            if (infoPtr->features&CURL_VERSION_HTTP2) {
                Tcl_ListObjAppendElement(interp,resultObjPtr
                        ,Tcl_NewStringObj("HTTP2",-1));
            }
	    break;
        case 4:
            resultObjPtr=Tcl_NewStringObj(infoPtr->ssl_version,-1);
            break;
        case 5:
            resultObjPtr=Tcl_NewLongObj(infoPtr->ssl_version_num);
            break;
        case 6:
            resultObjPtr=Tcl_NewStringObj(infoPtr->libz_version,-1);
            break;
        case 7:
            resultObjPtr=Tcl_NewListObj(0,(Tcl_Obj **)NULL);
            for(i=0;;i++) {
                if (infoPtr->protocols[i]!=NULL) {
                    Tcl_ListObjAppendElement(interp,resultObjPtr
                            ,Tcl_NewStringObj(infoPtr->protocols[i],-1));
                } else {
                    break;
                }
            }
    }

    Tcl_SetObjResult(interp,resultObjPtr);

    return TCL_OK;
}

#*
#*----------------------------------------------------------------------
#*
#* curlShareInitObjCmd --
#*
#*  This procedure is invoked to process the "curl::shareinit" Tcl command.
#*  See the user documentation for details on what it does.
#*
#* Results:
#*  A standard Tcl result.
#*
#* Side effects:
#*  See the user documentation.
#*
#*----------------------------------------------------------------------
#*/
critcl::ccommand ::curl::shareinit {cd interp objc objv} {

    Tcl_Obj               *resultPtr;
    CURL                  *shcurlHandle;
    struct shcurlObjData  *shcurlData;
    char                  *shandleName;

    if (objc != 1) {
	Tcl_WrongNumArgs(interp, 1, objv, "");
        return TCL_ERROR;
    }

    shcurlData=(struct shcurlObjData *)Tcl_Alloc(sizeof(struct shcurlObjData));
    if (shcurlData==NULL) {
        resultPtr=Tcl_NewStringObj("Couldn't allocate memory",-1);
        Tcl_SetObjResult(interp,resultPtr);
        return TCL_ERROR;
    }

    memset(shcurlData, 0, sizeof(struct shcurlObjData));

    shcurlHandle=curl_share_init();
    if (shcurlHandle==NULL) {
        resultPtr=Tcl_NewStringObj("Couldn't create share handle",-1);
        Tcl_SetObjResult(interp,resultPtr);
        return TCL_ERROR;
    }

    shandleName=curlCreateShareObjCmd(interp,shcurlData);

    shcurlData->shandle=shcurlHandle;

    resultPtr=Tcl_NewStringObj(shandleName,-1);
    Tcl_SetObjResult(interp,resultPtr);
    Tcl_Free(shandleName);

#ifdef TCL_THREADS
    curl_share_setopt(shcurlHandle, CURLSHOPT_LOCKFUNC, curlShareLockFunc);
    curl_share_setopt(shcurlHandle, CURLSHOPT_LOCKFUNC, curlShareUnLockFunc);
#endif

    return TCL_OK;
}

#*
#*----------------------------------------------------------------------
#*
#* curlEasyStringError --
#*
#*  This function is invoked to process the "curl::easystrerror" Tcl command.
#*  It will return a string with an explanation of the error code given.
#*
#* Results:
#*  A standard Tcl result.
#*
#* Side effects:
#*  The interpreter will contain as a result the string with the error
#*  message.
#*
#*----------------------------------------------------------------------
#*/
critcl::ccommand ::curl::easystrerror {cd interp objc objv} {


    if (objc != 2) {
        Tcl_WrongNumArgs(interp,1,objv,"errorCode");
        return TCL_ERROR;
    }

    if (curlErrorStrings(interp,objv[1],0)) {
        return TCL_ERROR;
    }
    return TCL_OK;
}

#*
#*----------------------------------------------------------------------
#*
#* curlShareStringError --
#*
#*  This function is invoked to process the "curl::sharestrerror" Tcl command.
#*  It will return a string with an explanation of the error code given.
#*
#* Results:
#*  A standard Tcl result.
#*
#* Side effects:
#*  The interpreter will contain as a result the string with the error
#*  message.
#*
#*----------------------------------------------------------------------
#*/
critcl::ccommand ::curl::sharestrerror {cd interp objc objv} {

    if (objc != 2) {
        Tcl_WrongNumArgs(interp,1,objv,"errorCode");
        return TCL_ERROR;
    }

    if (curlErrorStrings(interp,objv[1],1)) {
        return TCL_ERROR;
    }
    return TCL_OK;
}

#*
#*----------------------------------------------------------------------
#*
#* curlMultiStringError --
#*
#*  This function is invoked to process the "curl::multirerror" Tcl command.
#*  It will return a string with an explanation of the error code given.
#*
#* Results:
#*  A standard Tcl result.
#*
#* Side effects:
#*  The interpreter will contain as a result the string with the error
#*  message.
#*
#*----------------------------------------------------------------------
#*/
critcl::ccommand ::curl::multistrerror {cd interp objc objv} {

    if (objc != 2) {
        Tcl_WrongNumArgs(interp,1,objv,"errorCode");
        return TCL_ERROR;
    }

    if (curlErrorStrings(interp,objv[1],2)) {
        return TCL_ERROR;
    }
    return TCL_OK;
}

#/*
#*----------------------------------------------------------------------
#*
#* curlInitMultiObjCmd --
#*
#*	This procedure is invoked to process the "curl::multiinit" Tcl command.
#*	See the user documentation for details on what it does.
#*
#* Results:
#*	A standard Tcl result.
#*
#* Side effects:
#*	See the user documentation.
#*
#*----------------------------------------------------------------------
#*/
critcl::ccommand ::curl::multiinit {cd interp objc objv} {

    Tcl_Obj                     *result;
    struct curlMultiObjData     *curlMultiData;
    char                        *multiHandleName;

    if (objc != 1) {
	Tcl_WrongNumArgs(interp, 1, objv, "");
        return TCL_ERROR;
    }

    curlMultiData=(struct curlMultiObjData *)Tcl_Alloc(sizeof(struct curlMultiObjData));
    if (curlMultiData==NULL) {
        result=Tcl_NewStringObj("Couldn't allocate memory",-1);
        Tcl_SetObjResult(interp,result); 
        return TCL_ERROR;
    }

    memset(curlMultiData, 0, sizeof(struct curlMultiObjData));
    curlMultiData->interp=interp;

    curlMultiData->mcurl=curl_multi_init();

    if (curlMultiData->mcurl==NULL) {
        result=Tcl_NewStringObj("Couldn't open curl multi handle",-1);
        Tcl_SetObjResult(interp,result); 
        return TCL_ERROR;
    }

    multiHandleName=curlCreateMultiObjCmd(interp,curlMultiData);

    result=Tcl_NewStringObj(multiHandleName,-1);
    Tcl_SetObjResult(interp,result);
    Tcl_Free(multiHandleName);

    return TCL_OK;
}

package provide TclCurl 7.35.0
