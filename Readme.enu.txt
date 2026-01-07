**********************************************************************
                     CnPack For Delphi/C++Builder
                A Free, Open Source Project from China
                 (C)Copyright 2001-2026 CnPack Team
**********************************************************************

                    CnPack Component Package (Alpha)
                           Version 2026.01.01
                        =======================
                          CnPack Team 2026.01


=====================================================
1. Software Copyright Statement
=====================================================

    CnPack products is published in Open Source mode under CnPack 
License, and also, is protected by it.

    The License file describes it. Please see license file for details.

    Welcome to CnPack website to get latest information.

    https://www.cnpack.org

=====================================================
2. Structure
=====================================================

    It's not recommended to use Alpha package of CnPack in formal usage 
because it's not tested integrallty yet. If you meet any problem during
development, please contact us:

    https://www.cnpack.org
    mailto:master@cnpack.org

    There are several directories in CnPack source codes:
    1. Doc. It contains all documents of CnPack.
    2. Packages. It contains the package files needed by installation.
    3. Examples. It contains examples source codes.
    4. Source. It contains all source codes of CnPack package.

======================================================================
3. Installation
======================================================================

    Now Delphi 5/6/7/2005/2006/2007/2009/2010/XE/XE2/XE3/XE4/XE5/XE6/XE7/XE8/
10 Seattle/10.1 Berlin/10.2 Tokyo/10.3 Rio/10.4 Sydney/11 Alexandria/12 Athens/
13 Florence and C++Builder 5/6 are supported by CnPack Component Package.
FPC 3/Lazarus 4 are also partly supported.
    You can open corresponding package files in IDE and press "compile" 
or "install" buttons to compile and install it. The package files with "dcl"
prefix are for design time, others for run time.

    Note: please compile run-time package first, then install design-time package.

    Next, it is necessary to add each directory where the cnvcl source
code is located to Delphi's search paths. For lower-version Delphi, click
"Environment Options" under the "Tools" menu to open the settings dialog.
Then, click the button on the right side of the "Library Path" item on the
"Library" page, and add each subdirectory under the development package's
Source directory to both the "Library Path" and the "Browsing Path". For
higher-version Delphi, click "Options" under the "Tools" menu to open the
settings dialog. Select "Language", then "Delphi", and finally "Library".
Add each subdirectory under the development package's Source directory to
both the "Library Path" and the "Browsing Path".

    If you need other languages of CnPack Component Package. You can copy
the consts files from "Source\Lang\Language id" directory and overwrite 
those files in Source's subdirectory and rebuild the package. Also you can
run the "ToENU.bat" or other bat files in "Source" directory to copy
them automatically.

    Note: It's not recommended to use Alpha package of CnPack in formal
usage because it's not tested integrallty yet.
