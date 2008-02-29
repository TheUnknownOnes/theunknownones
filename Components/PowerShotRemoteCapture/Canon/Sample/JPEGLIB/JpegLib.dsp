# Microsoft Developer Studio Project File - Name="JpegLib" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** 編集しないでください **

# TARGTYPE "Win32 (x86) Static Library" 0x0104

CFG=JpegLib - Win32 Debug
!MESSAGE これは有効なﾒｲｸﾌｧｲﾙではありません。 このﾌﾟﾛｼﾞｪｸﾄをﾋﾞﾙﾄﾞするためには NMAKE を使用してください。
!MESSAGE [ﾒｲｸﾌｧｲﾙのｴｸｽﾎﾟｰﾄ] ｺﾏﾝﾄﾞを使用して実行してください
!MESSAGE 
!MESSAGE NMAKE /f "JpegLib.mak".
!MESSAGE 
!MESSAGE NMAKE の実行時に構成を指定できます
!MESSAGE ｺﾏﾝﾄﾞ ﾗｲﾝ上でﾏｸﾛの設定を定義します。例:
!MESSAGE 
!MESSAGE NMAKE /f "JpegLib.mak" CFG="JpegLib - Win32 Debug"
!MESSAGE 
!MESSAGE 選択可能なﾋﾞﾙﾄﾞ ﾓｰﾄﾞ:
!MESSAGE 
!MESSAGE "JpegLib - Win32 Release" ("Win32 (x86) Static Library" 用)
!MESSAGE "JpegLib - Win32 Debug" ("Win32 (x86) Static Library" 用)
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
RSC=rc.exe

!IF  "$(CFG)" == "JpegLib - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_MBCS" /D "_LIB" /YX /FD /c
# ADD CPP /nologo /Zp1 /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_MBCS" /D "_LIB" /YX /FD /c
# ADD BASE RSC /l 0x411 /d "NDEBUG"
# ADD RSC /l 0x411 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo
# Begin Special Build Tool
SOURCE="$(InputPath)"
PostBuild_Cmds=COPY Release\JpegLib.lib ..\RelCtrl
# End Special Build Tool

!ELSEIF  "$(CFG)" == "JpegLib - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Target_Dir ""
# ADD BASE CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_MBCS" /D "_LIB" /YX /FD /GZ /c
# ADD CPP /nologo /Zp1 /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_MBCS" /D "_LIB" /YX /FD /GZ /c
# ADD BASE RSC /l 0x411 /d "_DEBUG"
# ADD RSC /l 0x411 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LIB32=link.exe -lib
# ADD BASE LIB32 /nologo
# ADD LIB32 /nologo
# Begin Special Build Tool
SOURCE="$(InputPath)"
PostBuild_Cmds=COPY Debug\JpegLib.lib ..\RelCtrl
# End Special Build Tool

!ENDIF 

# Begin Target

# Name "JpegLib - Win32 Release"
# Name "JpegLib - Win32 Debug"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat"
# Begin Source File

SOURCE=.\jcapimin.cpp
# End Source File
# Begin Source File

SOURCE=.\jcapistd.cpp
# End Source File
# Begin Source File

SOURCE=.\jccoefct.cpp
# End Source File
# Begin Source File

SOURCE=.\jccolor.cpp
# End Source File
# Begin Source File

SOURCE=.\jcdctmgr.cpp
# End Source File
# Begin Source File

SOURCE=.\jchuff.cpp
# End Source File
# Begin Source File

SOURCE=.\jcinit.cpp
# End Source File
# Begin Source File

SOURCE=.\jcmainct.cpp
# End Source File
# Begin Source File

SOURCE=.\jcmarker.cpp
# End Source File
# Begin Source File

SOURCE=.\jcmaster.cpp
# End Source File
# Begin Source File

SOURCE=.\jcomapi.cpp
# End Source File
# Begin Source File

SOURCE=.\jcparam.cpp
# End Source File
# Begin Source File

SOURCE=.\jcphuff.cpp
# End Source File
# Begin Source File

SOURCE=.\jcprepct.cpp
# End Source File
# Begin Source File

SOURCE=.\jcsample.cpp
# End Source File
# Begin Source File

SOURCE=.\jctrans.cpp
# End Source File
# Begin Source File

SOURCE=.\jdapimin.cpp
# End Source File
# Begin Source File

SOURCE=.\jdapistd.cpp
# End Source File
# Begin Source File

SOURCE=.\jdatadst.cpp
# End Source File
# Begin Source File

SOURCE=.\jdatasrc.cpp
# End Source File
# Begin Source File

SOURCE=.\jdcoefct.cpp
# End Source File
# Begin Source File

SOURCE=.\jdcolor.cpp
# End Source File
# Begin Source File

SOURCE=.\jddctmgr.cpp
# End Source File
# Begin Source File

SOURCE=.\jdhuff.cpp
# End Source File
# Begin Source File

SOURCE=.\jdinput.cpp
# End Source File
# Begin Source File

SOURCE=.\jdmainct.cpp
# End Source File
# Begin Source File

SOURCE=.\jdmarker.cpp
# End Source File
# Begin Source File

SOURCE=.\jdmaster.cpp
# End Source File
# Begin Source File

SOURCE=.\jdmerge.cpp
# End Source File
# Begin Source File

SOURCE=.\jdphuff.cpp
# End Source File
# Begin Source File

SOURCE=.\jdpostct.cpp
# End Source File
# Begin Source File

SOURCE=.\jdsample.cpp
# End Source File
# Begin Source File

SOURCE=.\jdtrans.cpp
# End Source File
# Begin Source File

SOURCE=.\jerror.cpp
# End Source File
# Begin Source File

SOURCE=.\jfdctflt.cpp
# End Source File
# Begin Source File

SOURCE=.\jfdctfst.cpp
# End Source File
# Begin Source File

SOURCE=.\jfdctint.cpp
# End Source File
# Begin Source File

SOURCE=.\jidctflt.cpp
# End Source File
# Begin Source File

SOURCE=.\jidctfst.cpp
# End Source File
# Begin Source File

SOURCE=.\jidctint.cpp
# End Source File
# Begin Source File

SOURCE=.\jidctred.cpp
# End Source File
# Begin Source File

SOURCE=.\jmemmgr.cpp
# End Source File
# Begin Source File

SOURCE=.\jmemnobs.cpp
# End Source File
# Begin Source File

SOURCE=.\jquant1.cpp
# End Source File
# Begin Source File

SOURCE=.\jquant2.cpp
# End Source File
# Begin Source File

SOURCE=.\jutils.cpp
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl"
# Begin Source File

SOURCE=.\cderror.h
# End Source File
# Begin Source File

SOURCE=.\cdjpeg.h
# End Source File
# Begin Source File

SOURCE=.\jchuff.h
# End Source File
# Begin Source File

SOURCE=.\jconfig.h
# End Source File
# Begin Source File

SOURCE=.\jdct.h
# End Source File
# Begin Source File

SOURCE=.\jdhuff.h
# End Source File
# Begin Source File

SOURCE=.\jerror.h
# End Source File
# Begin Source File

SOURCE=.\jinclude.h
# End Source File
# Begin Source File

SOURCE=.\jmemsys.h
# End Source File
# Begin Source File

SOURCE=.\jmorecfg.h
# End Source File
# Begin Source File

SOURCE=.\jpegint.h
# End Source File
# Begin Source File

SOURCE=.\jpeglib.h
# End Source File
# Begin Source File

SOURCE=.\jversion.h
# End Source File
# Begin Source File

SOURCE=.\StdAfx.h
# End Source File
# Begin Source File

SOURCE=.\transupp.h
# End Source File
# End Group
# End Target
# End Project
