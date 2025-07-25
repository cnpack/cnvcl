{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2025 CnPack 开发组                       }
{                   ------------------------------------                       }
{                                                                              }
{            本开发包是开源的自由软件，您可以遵照 CnPack 的发布协议来修        }
{        改和重新发布这一程序。                                                }
{                                                                              }
{            发布这一开发包的目的是希望它有用，但没有任何担保。甚至没有        }
{        适合特定目的而隐含的担保。更详细的情况请参阅 CnPack 发布协议。        }
{                                                                              }
{            您应该已经和开发包一起收到一份 CnPack 发布协议的副本。如果        }
{        还没有，可访问我们的网站：                                            }
{                                                                              }
{            网站地址：https://www.cnpack.org                                  }
{            电子邮件：master@cnpack.org                                       }
{                                                                              }
{******************************************************************************}

unit CnCommon;
{* |<PRE>
================================================================================
* 软件名称：开发包基础库
* 单元名称：公共运行基础库单元
* 单元作者：CnPack 开发组
* 备    注：该单元定义了组件包的基础类库
* 开发平台：PWin98SE + Delphi 5.0
* 兼容测试：PWin9X/2000/XP + Delphi 5/6
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 修改记录：2025.06.23
*               在 FPC3 下编译通过
*           2022.10.07 by LiuXiao
*               增加多行文本输入框函数
*           2021.06.22 by LiuXiao
*               增加多按钮提示框的函数，暂不支持图标
*           2019.02.25 by LiuXiao
*               增加检查中国大陆 18 位身份证号是否正确的函数
*           2012.01.19 by LiuXiao
*               增加一个移植自外国牛人的快速开根号倒数的函数
*           2011.11.02 by LiuXiao
*               增加来自于 ccrun 的把程序钉到 Win7 任务栏的函数
*           2011.10.03 by LiuXiao
*               增加一部分封装对 Control 操作的函数过程以对付 FMX 框架
*           2007.01.31 by LiuXiao
*               增加获取一对象所有属性列表的函数
*           2006.11.29 by shenloqi
*               修改了 ShortNameToLongName 函数，使其支持 Win95/NT（不支持 Linux）
*           2005.08.02 by shenloqi
*               增加了 SameCharCounts，CharCounts ，RelativePath函数，重写了
*               GetRelativePath函数
*           2005.07.08 by shenloqi
*               修改了 GetRelativePath 函数，修改了 FileMatchesExts 函数，增加了
*             一系列通配符支持的函数：FileNameMatch，MatchExt，MatchFileName，
*             FileExtsToStrings，FileMasksToStrings，FileMatchesMasks
*           2005.05.03 by hubdog
*               增加 ExploreFile 函数
*           2004.09.18 by Shenloqi
*               为 Delphi 5 增加了 BoolToStr 函数
*           2004.05.21 by Icebird
*               修改了函数 GetLine, IsInt, IsFloat, CnDateToStr, MyDateToStr
*           2003.10.29 by Shenloqi
*               新增四个函数CheckWinXP,DllGetVersion,GetSelText,UnQuotedStr
*           2002.08.12 V1.1
*               新增一个函数 CheckAppRunning by 周劲羽
*           2002.04.09 V1.0
*               整理单元，重设版本号
*           2002.03.17 V0.02
*               新增部分函数，并部分修改
*           2002.01.30 V0.01
*               创建单元（整理而来）
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, TypInfo, IniFiles,
{$IFDEF MSWINDOWS} // 如果 Windows 下编译错误找不到 ComCtrls 单元，请在编译选项里加 Vcl 前缀
  Windows, Messages, Graphics, Controls, Forms, Dialogs,
  ComCtrls, Math, Menus, Registry, ComObj, FileCtrl, ShellAPI, CommDlg,
  MMSystem, StdCtrls, ExtCtrls, ActiveX, ShlObj, CheckLst, MultiMon,
  {$IFDEF FPC} Variants, JwaTlHelp32, JwaPsApi, {$ELSE} TLHelp32, PsAPI, {$ENDIF}
  {$IFDEF COMPILER6_UP}
    Types,
  {$ENDIF}
{$ELSE}
  System.Types, System.UITypes, System.Math, System.IOUtils, Posix.SysStat,
  FMX.ImgList, FMX.Graphics, FMX.ListView, FMX.ListBox, FMX.Menus, FMX.Memo,
  FMX.Forms, FMX.Controls, FMX.Edit, FMX.ListView.Types, FMX.Dialogs,
{$ENDIF}
{$IFDEF COMPILER6_UP}
  StrUtils, Variants,
{$ENDIF}
  CnConsts, CnNative, CnIni, CnIniStrUtils, CnWideStrings;

const
{$IFNDEF COMPILER6_UP}
  sLineBreak = {$IFDEF POSIX} #10 {$ENDIF} {$IFDEF MSWINDOWS} #13#10 {$ENDIF};
{$ENDIF}

  Alpha = ['A'..'Z', 'a'..'z', '_'];
  AlphaNumeric = Alpha + ['0'..'9'];

  SCN_UTF16_ANSI_WIDE_CHAR_SEP = $900;

//------------------------------------------------------------------------------
// 公共类型定义
//------------------------------------------------------------------------------

type
  PRGBColor = ^TRGBColor;
  TRGBColor = packed record
    b, g, r: Byte;
  end;

  PRGBArray = ^TRGBArray;
  TRGBArray = array[0..65535] of TRGBColor;

  ENotImplementedException = class(Exception);

  TCnDlgButtonCaption = (cdbCancel, cdbOK, cdbNo, cdbYes, cdbNoToAll, cdbYesToAll);
  {* 自定义多按钮对话框的按钮值}

  TCnDlgResult = (cdrCancel, cdrOK, cdrNo, cdrYes, cdrNoToAll, cdrYesToAll);
  {* 自定义多按钮对话框的返回值}

  TCnDlgButtonCaptions = set of TCnDlgButtonCaption;
  TCnDlgResults = set of TCnDlgResult;

{$IFDEF COMPILER5}
  UTF8String = type string; // Delphi 5 has no UTF8String definition
{$ENDIF}

  TCnSenderCallback = procedure(Sender: TObject);

  TCnIdentWordStyle = (iwsUpperCase, iwsLowerCase, iwsUpperFirstChar);

//==============================================================================
// Ansi 字符串函数
//==============================================================================

function AnsiTrim(const S: AnsiString): AnsiString;

function TrimBom(const S: AnsiString): AnsiString;

//------------------------------------------------------------------------------
// 扩展的文件目录操作函数
//------------------------------------------------------------------------------

{$IFDEF MSWINDOWS}

procedure ExploreDir(const APath: string; ShowDir: Boolean = True);
{* 在资源管理器中打开指定目录 }

procedure ExploreFile(const AFile: string; ShowDir: Boolean = True);
{* 在资源管理器中打开指定文件 }

function ForceDirectories(Dir: string): Boolean;
{* 递归创建多级子目录}

function MoveFile(const sName, dName: string): Boolean;
{* 移动文件、目录，参数为源、目标名}

function DeleteToRecycleBin(const FileName: string): Boolean;
{* 删除文件到回收站}

procedure FileProperties(const FName: string);
{* 打开文件属性窗口}

function OpenDialog(var FileName: string; const Title: string; const Filter: string;
  const Ext: string): Boolean;
{* 打开文件框}

function GetDirectory(const Caption: string; var Dir: string;
  ShowNewButton: Boolean = True): Boolean;
{* 显示选择文件夹对话框，支持设置默认文件夹}

function SelectDirectoryW(hOwn: HWND; var Path: WideString; const Caption,
  Root: WideString; uFlag: DWORD = $25): Boolean;
{* 类似 SelectDirectory 函数的支持 Unicode 的选择文件夹}

function SelectDirectoryEx(hOwn: HWND; const Caption, Root: WideString): WideString;
{* 对 SelectDirectoryW 的封装}

function FormatPath(const APath: string; Width: Integer): string;
{* 缩短显示不下的长路径名}

procedure DrawCompactPath(Hdc: HDC; Rect: TRect; const Str: string);
{* 通过 DrawText 来画缩略路径}

procedure DrawMatchText(Canvas: TCanvas; const MatchStr, Text: string;
  X, Y: Integer; HighlightColor: TColor; MatchedIndexes: TList = nil;
  StartOffset: Integer = 1);
{* 在指定 Canvas 上绘制匹配的字符串，匹配部分高亮显示}

{$ENDIF}

function SameCharCounts(S1, S2: string): Integer;
{* 两个字符串的前面的相同字符数}
function CharCounts(Str: PChar; Chr: Char): Integer;
{* 在字符串中某字符出现的次数}
function GetRelativePath(ATo, AFrom: string;
  const PathStr: string = '\'; const ParentStr: string = '..';
  const CurrentStr: string = '.'; const UseCurrentDir: Boolean = False): string;
{* 取两个目录的相对路径}

function CombineCommonPath(InFiles, OutFiles: TStrings): Boolean;
{* 寻找一批文件的共同根路径，并将去除共同根路径的文件名放至 OutFiles 中，返回是否去除成功}

{$IFNDEF BCB}
function PathRelativePathToA(pszPath: PAnsiChar; pszFrom: PAnsiChar; dwAttrFrom: DWORD;
  pszTo: PAnsiChar; dwAttrTo: DWORD): BOOL; stdcall;
function PathRelativePathToW(pszPath: PWideChar; pszFrom: PWideChar; dwAttrFrom: DWORD;
  pszTo: PWideChar; dwAttrTo: DWORD): BOOL; stdcall;
function PathRelativePathTo(pszPath: PChar; pszFrom: PChar; dwAttrFrom: DWORD;
  pszTo: PChar; dwAttrTo: DWORD): BOOL; stdcall;

function RelativePath(const AFrom, ATo: string; FromIsDir, ToIsDir: Boolean): string;
{* 使用 Windows API 取两个目录的相对路径}
{$ENDIF}

function LinkPath(const Head, Tail: string): string;
{* 连接两个路径，
   Head - 首路径，可以是 C:\Test、\\Test\C\Abc、http://www.abc.com/dir/ 等格式
   Tail - 尾路径，可以是 ..\Test、Abc\Temp、\Test、/web/lib 等格式或绝对地址格式 }

{$IFDEF MSWINDOWS}

procedure RunFile(const FName: string; Handle: THandle = 0;
  const Param: string = '');
{* 运行一个文件}

procedure OpenUrl(const Url: string; UseCmd: Boolean = False);
{* 打开一个链接}

procedure MailTo(const Addr: string; const Subject: string = ''; UseCmd: Boolean = False);
{* 发送邮件}

procedure SendMailTo(const Subject, Body, Addr: string; Attachs: array of string);
{* 使用 MSAPI 发送邮件，支持附件，但要求系统安装了 MSAPI}

function WinExecute(const FileName: string; Visibility: Integer = SW_NORMAL): Boolean;
{* 运行一个文件并立即返回 }

function WinExecAndWait32(const FileName: string; Visibility: Integer = SW_NORMAL;
  ProcessMsg: Boolean = False): Integer;
{* 运行一个文件并等待其结束}

function WinExecWithPipe(const CmdLine, Dir: string; slOutput: TStrings;
  var dwExitCode: Cardinal): Boolean; overload;
{* 用管道方式在 Dir 目录执行 CmdLine，Output 返回输出信息，较为实时，无需等待执行完毕
  便可在 slOutput 中陆续返回值，dwExitCode 返回退出码。如果成功返回 True }

function WinExecWithPipe(const CmdLine, Dir: string; var Output: string;
  var dwExitCode: Cardinal): Boolean; overload;
{* 用管道方式在 Dir 目录执行 CmdLine，Output 返回输出信息，等待执行完毕后再返回结果值
   dwExitCode 返回退出码。如果成功返回 True }

function CreateGuidString: string;
{* 创建 GUID 字符串}

function AppPath: string;
{* 应用程序路径}

function ModulePath: string;
{* 当前执行模块所在的路径 }

function GetProgramFilesDir: string;
{* 取 Program Files 目录}

function GetWindowsDir: string;
{* 取 Windows 目录}

function GetWindowsTempPath: string;
{* 取临时文件路径}

function CnGetTempFileName(const Ext: string): string;
{* 返回一个临时文件名 }

function GetSystemDir: string;
{* 取系统目录}

function GetMyDocumentsDir: string;
{* 取我的文档目录}

function ShortNameToLongName(const FileName: string): string;
{* 短文件名转长文件名}

function LongNameToShortName(const FileName: string): string;
{* 长文件名转短文件名}

function GetTrueFileName(const FileName: string): string;
{* 取得真实长文件名，包含大小写}

function FindExecFile(const AName: string; var AFullName: string): Boolean;
{* 查找可执行文件的完整路径 }

function GetSpecialFolderLocation(const Folder: Integer): string;
{* 取得系统特殊文件夹位置，Folder 使用在 ShlObj 中定义的标识，如 CSIDL_DESKTOP }

{$ENDIF}

function AddDirSuffix(const Dir: string): string;
{* 目录尾加 '\' 修正}

function MakePath(const Dir: string): string;
{* 目录尾加 '\' 修正}

function MakeDir(const Path: string): string;
{* 路径尾去掉 '\'}

function GetUnixPath(const Path: string): string;
{* 路径中的 '\' 转成 '/'}

function GetWinPath(const Path: string): string;
{* 路径中的 '/' 转成 '\'}

function FileNameMatch(Pattern, FileName: PAnsiChar): Integer;
{* 文件名是否与通配符匹配，返回值为 0 表示匹配，其他为不匹配}

function MatchExt(const S, Ext: string): Boolean;
{* 文件名是否与扩展名通配符匹配}

function MatchFileName(const S, FN: string): Boolean;
{* 文件名是否与通配符匹配}

procedure FileExtsToStrings(const FileExts: string; ExtList: TStrings; CaseSensitive: Boolean);
{* 转换扩展名通配符字符串为通配符列表}

function FileMatchesExts(const FileName, FileExts: string; CaseSensitive: Boolean = False): Boolean; overload;
function FileMatchesExts(const FileName: string; ExtList: TStrings): Boolean; overload;
{* 文件名是否匹配扩展名通配符。FileExts 是如 '.pas;.dfm;.inc' 这样的字符串}

procedure FileMasksToStrings(const FileMasks: string; MaskList: TStrings; CaseSensitive: Boolean);
{* 转换文件通配符字符串为通配符列表}

function FileMatchesMasks(const FileName, FileMasks: string; CaseSensitive: Boolean): Boolean; overload;
function FileMatchesMasks(const FileName: string; MaskList: TStrings): Boolean; overload;
{* 文件名是否匹配通配符}

{$IFDEF MSWINDOWS}

function IsFileInUse(const FName: string): Boolean;
{* 判断文件是否正在使用}

function IsAscii(const FileName: string): Boolean;
{* 判断文件是否为 Ascii 文件}

function IsValidFileName(const Name: string): Boolean;
{* 判断文件是否是有效的文件名}

function GetValidFileName(const Name: string): string;
{* 返回有效的文件名 }

function SetFileDate(const FileName: string; CreationTime, LastWriteTime, LastAccessTime:
  TFileTime): Boolean;
{* 设置文件时间}

function GetFileDate(const FileName: string; var CreationTime, LastWriteTime, LastAccessTime:
  TFileTime): Boolean;
{* 取文件时间}

function FileTimeToDateTime(const FileTime: TFileTime): TDateTime;
{* 文件时间转本地日期时间}

function DateTimeToFileTime(const DateTime: TDateTime): TFileTime;
{* 本地日期时间转文件时间}

function GetFileIcon(const FileName: string; var Icon: TIcon): Boolean;
{* 取得与文件相关的图标，成功则返回 True}

function CreateBakFile(const FileName, Ext: string): Boolean;
{* 创建备份文件}

function FileTimeToLocalSystemTime(FTime: TFileTime): TSystemTime;
{* 文件时间转本地时间}

function LocalSystemTimeToFileTime(STime: TSystemTime): TFileTime;
{* 本地时间转文件时间}

function DateTimeToLocalDateTime(DateTime: TDateTime): TDateTime;
{* UTC 时间转本地时间}

function LocalDateTimeToDateTime(DateTime: TDateTime): TDateTime;
{* 本地时间转 UTC 时间}

procedure PinAppToWin7Taskbar(const Path, App: string);
{* 把程序钉到 Windows 7 任务栏，参数为程序路径与文件名}

{$ENDIF}

{$IFDEF COMPILER5}
type
  TValueRelationship = -1..1;

function CompareValue(const A, B: Int64): TValueRelationship;

function AnsiStartsText(const ASubText, AText: string): Boolean;
{* AText 是否以 ASubText 开头 }

function AnsiReplaceText(const AText, AFromText, AToText: string): string;
{$ENDIF}

function ReplaceAllInString(const S: string; OldPattern, NewPattern: string): string;
{* StringReplace 无法处理字符串中的 #0，新写一个全替换的版本，不处理大小写}

{$IFNDEF COMPILER7_UP}
function AnsiContainsText(const AText, ASubText: string): Boolean;
{* AText 是否包含 ASubText }
{$ENDIF}

function AnsiCompareTextPos(const ASubText, AText1, AText2: string): TValueRelationship;
function CompareTextPos(const ASubText, AText1, AText2: string): TValueRelationship;
{* 比较 SubText 在两个字符串中出现的位置的大小，如果相等则比较字符串本身，忽略大小写 }

function CompareTextWithPos(const ASubText, AText1, AText2: string;
  Reverse: Boolean): TValueRelationship;
{* 比较 SubText 在两个字符串中出现的位置的大小，位置不反序，
   相等则比较字符串本身，本身可反序，忽略大小写 }

function StringReplaceNonAnsi(const S, OldPattern, NewPattern: string;
  Flags: TReplaceFlags): string;
{* 非 Ansi 方式的字符串替换}

function StringKMP(const Pattern, S: string): Integer;
{* KMP 匹配算法，返回 S 中第一次出现 Pattern 的位置，位置以 1 开始，未匹配则返回 0}

function Deltree(const Dir: string; DelRoot: Boolean = True;
  DelEmptyDirOnly: Boolean = False): Boolean;
{* 删除整个目录, DelRoot 表示是否删除目录本身}

procedure DelEmptyTree(const Dir: string; DelRoot: Boolean = True);
{* 删除整个目录中的空目录, DelRoot 表示是否删除目录本身}

function GetDirFiles(const Dir: string; FileNames: TStrings = nil): Integer;
{* 取文件夹下的直系文件列表，文件名不包含路径名。不搜索子目录。返回文件数}

{$IFDEF MSWINDOWS}

type
  TFindCallBack = procedure(const FileName: string; const Info: TSearchRec;
    var Abort: Boolean) of object;
{* 查找指定目录下文件的回调函数}

  TDirCallBack = procedure(const SubDir: string) of object;
{* 查找指定目录时进入子目录回调函数}

function FindFile(const Path: string; const FileName: string = '*.*';
  Proc: TFindCallBack = nil; DirProc: TDirCallBack = nil; bSub: Boolean = True;
  bMsg: Boolean = True): Boolean;
{* 查找指定目录下文件，返回是否被中断 }

function CnSearchFile(const FileName: string; const Ext: string = '.exe'): string;
{* 调用 SearchFile 在当前目录、系统目录和 PATH 变量中查找指定扩展名的文件。
   FileName 为不带扩展名的文件名，Ext 为扩展名，成功返回全路径文件名，失败返回空。}

function OpenWith(const FileName: string): Integer;
{* 显示文件打开方式对话框}

function CheckAppRunning(const FileName: string; var Running: Boolean): Boolean;
{* 检查指定的应用程序是否正在运行
 |<PRE>
   const FileName: string   - 应用程序文件名，不带路径，如果不带扩展名，
                              默认为".EXE"，大小写无所谓。
                              如 Notepad.EXE
   var Running: Boolean     - 返回该应用程序是否运行，运行为 True
   Result: Boolean          - 如果查找成功返回为 True，否则为 False
 |</PRE>}

type
  TVersionNumber = packed record
  {* 文件版本号}
    Major: Word;
    Minor: Word;
    Release: Word;
    Build: Word;
  end;

function GetFileVersionNumber(const FileName: string): TVersionNumber;
{* 取文件版本号}

function GetFileVersionStr(const FileName: string): string;
{* 取文件版本字符串}

function GetFileInfo(const FileName: string; var FileSize: Int64;
  var FileTime: TDateTime): Boolean;
{* 取文件信息}

function GetFileSize(const FileName: string): Int64;
{* 取文件长度}

function GetFileDateTime(const FileName: string): TDateTime;
{* 取文件 Delphi 格式日期时间}

{$ENDIF}

function LoadStringFromFile(const FileName: string): string;
{* 将文件读为字符串}

function SaveStringToFile(const S, FileName: string): Boolean;
{* 保存字符串到为文件}

procedure QuickSortStringList(List: TStringList; L, R: Integer; SCompare: TStringListSortCompare);
{* StringList 快排，弥补 D5、6 下排序不开放的局限}

//------------------------------------------------------------------------------
// 环境变量相关
//------------------------------------------------------------------------------

{$IFDEF MSWINDOWS}

function DelEnvironmentVar(const Name: string): Boolean;
{* 删除当前进程中的环境变量 }

function ExpandEnvironmentVar(var Value: string): Boolean;
{* 扩展当前进程中的环境变量 }

function GetEnvironmentVar(const Name: string; var Value: string;
  Expand: Boolean): Boolean;
{* 返回当前进程中的环境变量 }

function GetEnvironmentVars(const Vars: TStrings; Expand: Boolean): Boolean;
{* 返回当前进程中的环境变量列表 }

function SetEnvironmentVar(const Name, Value: string): Boolean;
{* 设置当前进程中的环境变量 }

{$ENDIF}

//------------------------------------------------------------------------------
// 扩展的字符串操作函数
//------------------------------------------------------------------------------

type
  TAnsiCharSet = set of AnsiChar;

  TCharSet = set of AnsiChar;

function CharInSet(C: Char; CharSet: TAnsiCharSet): Boolean;
{* 判断字符是否在集合内}

function InStr(const sShort: string; const sLong: string): Boolean;
{* 判断 sShort 是否包含在 sLong 中，不区分大小写}

function IntToStrEx(Value: Integer; Len: Integer; FillChar: Char = '0'): string;
{* 扩展整数转字符串函数}

function IntToStrSp(Value: Integer; SpLen: Integer = 3; Sp: Char = ',';
  ShowPlus: Boolean = False): string;
{* 带分隔符的整数－字符转换}

function IsFloat(const S: String): Boolean;
{* 判断字符串是否可转换成浮点型}

function IsInt(const S: String): Boolean;
{* 判断字符串是否可转换成整型}

function IsDateTime(const S: string): Boolean;
{* 判断字符串是否可转换成 DateTime }

function IsValidEmail(const S: string): Boolean;
{* 判断是否有效的邮件地址}

function IsSimpleFormat(const S: string): Boolean;
{* 判断是否是简单的格式化字符串}

function StrSpToInt(const Value: string; Sp: Char = ','): Int64;
{* 去掉字符串中的分隔符－字符转换}

function AverageNoOverflow(A, B: Integer): Integer;
{* 以不溢出的方式计算两个整型的算术平均数}

function ByteToBin(Value: Byte): string;
{* 字节转二进制串}

function WordToBin(Value: Word): string;
{* 双字节转二进制串}

function DWordToBin(Value: DWORD): string;
{* 四字节转二进制串}

function StrRight(const Str: string; Len: Integer): string;
{* 返回字符串右边的字符}

function StrLeft(const Str: string; Len: Integer): string;
{* 返回字符串左边的字符}

function StrEndWith(const S, Tail: string): Boolean;
{* 返回字符串是否以特定子串结尾}

function StrReverse(const S: string): string;
{* 翻转字符串}

function GetLine(C: Char; Len: Integer): string;
{* 返回字符串行}

function GetTextFileLineCount(const FileName: String): Integer;
{* 返回文本文件的行数}

function Spc(Len: Integer): string;
{* 返回空格串}

procedure SwapStr(var S1, S2: string);
{* 交换字串}

procedure SeparateStrAndNum(const AInStr: string; var AOutStr: string;
  var AOutNum: Integer);
{* 分割"非数字+数字"格式的字符串中的非数字和数字}

function UnQuotedStr(const Str: string; const Ch: Char;
  const Sep: string = ''): string;
{* 去除被引用的字符串的引用}

function LastStrPos(const SubStr, Str: string): Integer;
{* 查找字符串中最后一次出现的子串位置}

function LastCharPos(const S: string; C: Char): Integer;
{* 查找字符串中最后一次出现某字符的位置，无则返回 0}

function CharPosWithCounter(const Sub: Char; const AStr: String;
  Counter: Integer = 1): Integer;
{* 查找字符串中出现的第 Counter 次的字符的位置}

function CountCharInStr(const Sub: Char; const AStr: string): Integer;
{* 查找字符串中字符的出现次数}

function IsValidIdentChar(C: Char; First: Boolean = False): Boolean;
{* 判断字符是否有效标识符字符，First 表示是否为首字符}

function IsValidIdentW(const Ident: string): Boolean;
{* 判断字符串是否是有效的 Unicode 标识符，只在 Unicode 下调用}

function IsValidIdentWide(const Ident: WideString): Boolean;
{* 判断宽字符串是否是有效的 Unicode 标识符，只在 BDS 以上调用}

{$IFDEF COMPILER5}
function BoolToStr(B: Boolean; UseBoolStrs: Boolean = False): string;
{* Delphi5没有实现布尔型转换为字符串，类似于Delphi6,7的实现}
{$ENDIF COMPILER5}

function LinesToStr(const Lines: string): string;
{* 多行文本转单行（换行符转'\n'）}

function StrToLines(const Str: string): string;
{* 单行文本转多行（'\n'转换行符）}

function WideStrToLines(const Str: WideString): WideString;
{* 单行宽文本转多行（'\n'转换行符）}

function MyDateToStr(Date: TDate): string;
{* 日期转字符串，使用 yyyy.mm.dd 格式}

{$IFDEF MSWINDOWS}

function RegReadStringDef(const RootKey: HKEY; const Key, Name, Def: string): string;
{* 取注册表键值}

function GetKeysInRegistryKey(const Key: string; List: TStrings): Boolean;
{* 取注册表某键的子键列表}

{$ENDIF}

procedure ReadStringsFromIni(Ini: TCustomIniFile; const Section: string; Strings: TStrings);
{* 从 INI 中读取字符串列表}

procedure WriteStringsToIni(Ini: TCustomIniFile; const Section: string; Strings: TStrings);
{* 写字符串列表到 INI 文件中}

function VersionToStr(Version: DWORD): string;
{* 版本号转成字符串，如 $01020000 --> '1.2.0.0' }

function StrToVersion(const S: string): DWORD;
{* 字符串转成版本号，如 '1.2.0.0' --> $01020000，如果格式不正确，返回 $01000000 }

function CnDateToStr(Date: TDateTime): string;
{* 转换日期为 yyyy.mm.dd 格式字符串 }

function CnStrToDate(const S: string): TDateTime;
{* 将 yyyy.mm.dd 格式字符串转换为日期 }

function GetDatePart(DateTime: TDateTime): TDate;
{* 取日期时间的日期部分（整数）}

function GetTimePart(DateTime: TDateTime): TTime;
{* 取日期时间的时间部分（小数）}

function DateTimeToFlatStr(const DateTime: TDateTime): string;
{* 日期时间转 '20030203132345' 式样的 14 位数字字符串}

function FlatStrToDateTime(const Section: string; var DateTime: TDateTime): Boolean;
{* '20030203132345' 式样的 14 位数字字符串转日期时间}

function RMBFloatToChinese(ARMBCash: Real): string;
{* 数字转大写金额}

function EvalSimpleExpression(const Value: string): Double;
{* 计算四则运算与乘方的表达式值}

{$IFDEF MSWINDOWS}

function StrToRegRoot(const S: string): HKEY;
{* 字符串转注册表根键，支持 'HKEY_CURRENT_USER' 'HKCR' 长短两种格式}

function RegRootToStr(Key: HKEY; ShortFormat: Boolean = True): string;
{* 注册表根键转字符串，可选 'HKEY_CURRENT_USER' 'HKCR' 长短两种格式}

{$ENDIF}

function ExtractSubstr(const S: string; var Pos: Integer;
  const Delims: TSysCharSet): string;
{* 从字符串中根据指定的分隔符分离出子串
 |<PRE>
   const S: string           - 源字符串
   var Pos: Integer          - 输入查找的起始位置，输出查找完成的结束位置
   const Delims: TSysCharSet - 分隔符集合
   Result: string            - 返回子串
 |</PRE>}

function WildcardCompare(const FileWildcard, FileName: string; const IgnoreCase:
  Boolean = True): Boolean;
{* 文件名通配符比较}

{$IFDEF MSWINDOWS}

function ScanCodeToAscii(Code: Word): AnsiChar;
{* 根据当前键盘布局将键盘扫描码转换成 ASCII 字符，可在 WM_KEYDOWN 等处使用
   由于不调用 ToAscii，故可支持使用 Accent Character 的键盘布局 }

function IsDeadKey(Key: Word): Boolean;
{* 返回一个虚拟键是否 Dead key}

function VirtualKeyToAscii(Key: Word): AnsiChar;
{* 根据当前键盘状态将虚拟键转换成 ASCII 字符，可在 WM_KEYDOWN 等处使用
   可能会导致 Accent Character 不正确}

function VK_ScanCodeToAscii(VKey: Word; Code: Word): AnsiChar;
{* 根据当前的键盘布局将虚拟键和扫描码转换成 ASCII 字符。通过虚拟键来处理小键盘，
   扫描码处理大键盘，支持 Accent Character 的键盘布局 }

function GetShiftState: TShiftState;
{* 返回当前的按键状态，暂不支持 ssDouble 状态 }

function IsShiftDown: Boolean;
{* 判断当前 Shift 是否按下 }

function IsAltDown: Boolean;
{* 判断当前 Alt 是否按下 }

function IsCtrlDown: Boolean;
{* 判断当前 Ctrl 是否按下 }

function IsInsertDown: Boolean;
{* 判断当前 Insert 是否按下 }

function IsCapsLockDown: Boolean;
{* 判断当前 Caps Lock 是否按下 }

function IsNumLockDown: Boolean;
{* 判断当前 NumLock 是否按下 }

function IsScrollLockDown: Boolean;
{* 判断当前 Scroll Lock 是否按下 }

function HandleEditShortCut(AControl: TWinControl; AShortCut: TShortCut): Boolean;
{* 使控件处理标准编辑快捷键}

{$ENDIF}

function RemoveClassPrefix(const ClassName: string): string;
{* 删除类名前缀 T}

function CnAuthorEmailToStr(Author, Email: string): string;
{* 用分号分隔的作者、邮箱字符串转换为输出格式，例如：
 |<PRE>
   Author  = 'Tom;Jack;Bill'
   Email   = 'tom@email.com;jack@email.com;Bill@email.net'
   Result  = 'Tom(tom@email.com)' + #13#10 +
             'Jack(jack@email.com)' + #13#10 +
             'Bill(bill@email.net)
 |</PRE>}

//------------------------------------------------------------------------------
// 扩展的对话框函数
//------------------------------------------------------------------------------

{$IFDEF MSWINDOWS}

procedure InfoDlg(const Mess: string; Caption: string = ''; Flags: Integer
  = MB_OK + MB_ICONINFORMATION);
{* 显示提示窗口}

function InfoOk(const Mess: string; Caption: string = ''): Boolean;
{* 显示提示确认窗口}

procedure ErrorDlg(const Mess: string; Caption: string = '');
{* 显示错误窗口}

procedure WarningDlg(const Mess: string; Caption: string = '');
{* 显示警告窗口}

function QueryDlg(const Mess: string; DefaultNo: Boolean = False;
  Caption: string = ''): Boolean;
{* 显示查询是否窗口}

procedure LongMessageDlg(const Mess: string; AutoWrap: Boolean = False;
  const Caption: string = '');
{* 用 Memo 显示长字符串或多行字符串}

function MultiButtonsDlg(const Mess: string; Buttons: TCnDlgButtonCaptions;
  Caption: string = ''): TCnDlgResult;
{* 显示多按钮对话框，动态构造按钮并返回其 ModalResult，暂不支持图标
  注意 Buttons 里传入的 set 中的按钮顺序，需和 TCnDlgButtonCaption 中声明的一致}

{$ENDIF}

const
  csDefComboBoxSection = 'History';

{$IFDEF MSWINDOWS}

function CnInputQuery(const ACaption, APrompt: string;
  var Value: string; Ini: TCustomIniFile = nil;
  const Section: string = csDefComboBoxSection; APassword: Boolean = False;
  FormCallBack: TCnSenderCallback = nil): Boolean;
{* 输入单行字符串的对话框}

{$ENDIF}

function CnInputBox(const ACaption, APrompt, ADefault: string;
  Ini: TCustomIniFile = nil; const Section: string = csDefComboBoxSection;
  FormCallBack: TCnSenderCallback = nil): string;
{* 输入单行字符串的对话框，注意 FMX 下历史功能与回调无效，仅简单返回内容}

{$IFDEF MSWINDOWS}

function CnInputMultiLineQuery(const ACaption, APrompt: string;
  var Value: string; FormCallBack: TCnSenderCallback = nil): Boolean;
{* 输入多行字符串的对话框，返回 True 表示用户输入后点击 OK 了，输入内容在 Value 中}

function CnInputMultiLineBox(const ACaption, APrompt, ADefault: string;
  FormCallBack: TCnSenderCallback = nil): string;
{* 输入多行字符串的对话框，用户输入内容为空或 Cancel 时返回空字符串}

procedure CnShowHexData(Data: Pointer; DataByteLength: Integer; BaseAddr: Integer = 0;
  const ACaption: string = ''; Modal: Boolean = True);
{* 以十六进制的方式显示一块数据，Modal 参数控制对话框模态或非模态}

function CnSingleOptionQuery(const ACaption, APrompt: string; Options: TStrings;
  FormCallBack: TCnSenderCallback = nil): Integer;
{* 弹出对话框以单选按钮框 RadioGroup 显示多个选项让用户选择，返回选中的索引
  如用户未选择，或选择了取消，则返回 -1}

{$ENDIF}

//------------------------------------------------------------------------------
// 扩展日期时间操作函数
//------------------------------------------------------------------------------

function GetYear(Date: TDate): Integer;
{* 取日期年份分量}
function GetMonth(Date: TDate): Integer;
{* 取日期月份分量}
function GetDay(Date: TDate): Integer;
{* 取日期天数分量}
function GetHour(Time: TTime): Integer;
{* 取时间小时分量}
function GetMinute(Time: TTime): Integer;
{* 取时间分钟分量}
function GetSecond(Time: TTime): Integer;
{* 取时间秒分量}
function GetMSecond(Time: TTime): Integer;
{* 取时间毫秒分量}

//------------------------------------------------------------------------------
// 位操作函数
//------------------------------------------------------------------------------

type
  TByteBit = 0..7;
  {* Byte类型位数范围}
  TWordBit = 0..15;
  {* Word类型位数范围}
  TDWordBit = 0..31;
  {* DWord类型位数范围}

procedure SetBit(var Value: Byte; Bit: TByteBit; IsSet: Boolean); overload;
{* 设置二进制位}
procedure SetBit(var Value: WORD; Bit: TWordBit; IsSet: Boolean); overload;
{* 设置二进制位}
procedure SetBit(var Value: DWORD; Bit: TDWordBit; IsSet: Boolean); overload;
{* 设置二进制位}

function GetBit(Value: Byte; Bit: TByteBit): Boolean; overload;
{* 取二进制位}
function GetBit(Value: WORD; Bit: TWordBit): Boolean; overload;
{* 取二进制位}
function GetBit(Value: DWORD; Bit: TDWordBit): Boolean; overload;
{* 取二进制位}

function CountSetBits(const Value: Cardinal): Integer;
{* 计算二进制数字有多少位被置为 1}

//------------------------------------------------------------------------------
// 系统功能函数
//------------------------------------------------------------------------------

type
  PDLLVERSIONINFO = ^TDLLVERSIONINFO;
  TDLLVERSIONINFO = packed record
    cbSize: DWORD;
    dwMajorVersion: DWORD;
    dwMinorVersion: DWORD;
    dwBuildNumber: DWORD;
    dwPlatformId: DWORD;
  end;
  PDLLVERSIONINFO2 = ^TDLLVERSIONINFO2;
  TDLLVERSIONINFO2 = packed record
    info1: TDLLVERSIONINFO;
    dwFlags: DWORD;
    ullVersion: {$IFDEF MSWINDOWS} ULARGE_INTEGER {$ELSE} UInt64 {$ENDIF};
  end;

{$IFDEF MSWINDOWS}

procedure MoveMouseIntoControl(AWinControl: TControl);
{* 移动鼠标到控件}

procedure AddComboBoxTextToItems(ComboBox: TComboBox; MaxItemsCount: Integer = 10);
{* 将 ComboBox 的文本内容增加到下拉列表中}

function DynamicResolution(x, y: WORD): Boolean;
{* 动态设置分辨率}

procedure StayOnTop(Handle: HWND; OnTop: Boolean);
{* 窗口最上方显示}

procedure SetHidden(Hide: Boolean);
{* 设置程序是否出现在任务栏}

procedure SetTaskBarVisible(Visible: Boolean);
{* 设置任务栏是否可见}

procedure SetDesktopVisible(Visible: Boolean);
{* 设置桌面是否可见}

function CnSetWindowAlphaBlend(Hwnd: THandle; AlphaBlendValue: Byte): Boolean;
{* 设置窗体 Alpha 透明值}

function ForceForegroundWindow(HWND: HWND): Boolean;
{* 强制让一个窗口显示在前台}

function AllowGlobalFileDragMessage: Boolean;
{* Windows 10 下调用 ChangeWindowMessageFilter 设置允许接收低权限进程的拖拽等消息}

function AllowWindowFileDragMessage(HWnd: THandle): Boolean;
{* Windows 10 下调用 ChangeWindowMessageFilterEx 设置允许窗口接收低权限进程的拖拽等消息}

function GetWorkRect(const Form: TCustomForm = nil): TRect;
{* 取桌面区域}

procedure BeginWait;
{* 显示等待光标}

procedure EndWait;
{* 结束等待光标}

function CheckWindows9598: Boolean;
{* 检测是否 Windows 95/98 平台}

function CheckWindowsNT: Boolean;
{* 检测是否 WinNT 平台}

function CheckWinXP: Boolean;
{* 检测是否 Windows XP 或以上平台}

function CheckWinVista: Boolean;
{* 检查是否 Vista/Windows 7 或以上系统}

function CheckWin8: Boolean;
{* 检查是否 Windows 8 或以上系统}

function CheckWin10: Boolean;
{* 检查是否 Windows 10 或以上系统}

function CheckWin64: Boolean;
{* 检查是否是 64 位 Windows}

function CheckWow64: Boolean;
{* 检查当前进程是否是 32 位进程跑在 64 位子系统里}

function CheckProcess64(ProcessHandle: THandle = 0): Boolean;
{* 检查指定进程是否 64 位，参数为进程句柄（并非进程号）。如传 0 ，则判断当前进程}

function CheckProcessWow64(ProcessHandle: THandle): Boolean;
{* 检查指定进程是否是 32 位进程跑在 64 位子系统里。参数为进程句柄（并非进程号）}

function CheckXPManifest(var OSSupport, AppValid: Boolean): Boolean;
{* 检查系统和当前进程是否支持 XP Manifest
   OSSupport: 返回操作系统是否支持 XP Manifest
   AppValid: 返回当前进程是否启用了 XP Manifest
   Result: 检查是否成功 }

function DllGetVersion(const dllname: string;
  var DVI: TDLLVERSIONINFO2): Boolean;
{* 获得 Dll 的版本信息}

function GetOSString: string;
{* 返回操作系统标识串}

function GetComputeNameStr : string;
{* 得到本机名}

function GetLocalUserName: string;
{* 得到本机用户名}

function GetRegisteredCompany: string;
{* 得到公司名}

function GetRegisteredOwner: string;
{* 得到注册用户名}

//------------------------------------------------------------------------------
// 常用控件辅助过程
//------------------------------------------------------------------------------

procedure ListViewDeleteSelected(ListView: TListView);
{* 删除 ListView 当前选择项 }

procedure ListViewMoveDownSelected(ListView: TListView);
{* ListView 当前选择项向下移动 }

procedure ListViewMoveUpSelected(ListView: TListView);
{* ListView 当前选择项向上移动 }

procedure ListboxHorizontalScrollbar(Listbox: TCustomListBox);
{* 为 Listbox 增加水平滚动条}

procedure CloneMenuItem(Source, Dest: TMenuItem);
{* 复制菜单项和其子项}

function MemoGetCaretPos(Memo: TCustomMemo): TPoint;
{* 封装的获取 Memo 光标位置的方法，修补了 XE3 或以下碰到大文件时出负值的问题}

{$ENDIF}

procedure SelectMemoOneLine(AMemo: TMemo; FromLine: Integer);
{* 选中 Memo 的第 FromLine 后的一整行，首行是第 1 行}

//------------------------------------------------------------------------------
// 其它过程
//------------------------------------------------------------------------------

{$IFDEF MSWINDOWS}

function GetControlBitmap(AControl: TControl; Bmp: TBitmap; ResetSize: Boolean = False): Boolean;
{* 获取 Control 表面的位图，ResetSize 为 True 表示使用 Control 尺寸设置位图尺寸}

function GetMultiMonitorDesktopRect: TRect;
{* 获得多显示器情况下，整个桌面相对于主显示器原点的坐标}

{$ENDIF}

function TrimInt(Value, Min, Max: Integer): Integer;
{* 输出限制在 Min..Max 之间}

function CompareInt(V1, V2: Integer; Desc: Boolean = False): Integer;
{* 比较两个整数，V1 > V2 返回 1，V1 < V2 返回 -1，V1 = V2 返回 0
   如果 Desc 为 True，返回结果反向 }

function IntToByte(Value: Integer): Byte;
{* 输出限制在 0..255 之间}

function InBound(Value: Integer; V1, V2: Integer): Boolean;
{* 判断整数 Value 是否在 V1 和 V2 之间}

function SameMethod(Method1, Method2: TMethod): Boolean;
{* 比较两个方法地址是否相等}

function HalfFind(List: TList; P: Pointer; SCompare: TListSortCompare): Integer;
{* 二分法在排序列表中查找}

function CheckChineseIDCardNumber(const IDNumber: string): Boolean;
{* 检查中国大陆的 18 位身份证是否合法}

{$IFDEF MSWINDOWS}

procedure StretchDrawImageListToCanvas(ImageList: TImageList; ImageIndex: Integer;
  DestCanvas: TCanvas; X, Y, AWidth, AHeight: Integer);
{* 拉伸绘制 ImageList 中的指定图像至指定 Canvas 的指定矩形中，支持背景透明}

{$ENDIF}

type
  TFindRange = record
    tgFirst: Integer;
    tgLast: Integer;
  end;

function HalfFindEx(List: TList; P: Pointer; SCompare: TListSortCompare): TFindRange;
{* 二分法在排序列表中查找，支持重复记录，返回一个范围值}

procedure CnSwap(var A, B: Byte); overload;
{* 交换两个数}
procedure CnSwap(var A, B: Integer); overload;
{* 交换两个数}
procedure CnSwap(var A, B: Single); overload;
{* 交换两个数}
procedure CnSwap(var A, B: Double); overload;
{* 交换两个数}

function RectEqu(Rect1, Rect2: TRect): Boolean;
{* 比较两个 Rect 是否相等}

procedure DeRect(Rect: TRect; var x, y, Width, Height: Integer);
{* 分解一个 TRect 为左上角坐标 x, y 和宽度 Width、高度 Height}

function EnSize(cx, cy: Integer): TSize;
{* 返回一个 TSize 类型}

function RectWidth(Rect: TRect): Integer;
{* 计算 TRect 的宽度}

function RectHeight(Rect: TRect): Integer;
{* 计算 TRect 的高度}

{$IFDEF MSWINDOWS}

procedure Delay(const uDelay: DWORD);
{* 延时}

procedure SetClipboardContent(Format: Word; var Buffer; Size: Integer);
{* 把指定内存内容以指定格式设置入剪贴板}

{$IFNDEF WIN64}
procedure BeepEx(const Freq: WORD = 1200; const Delay: WORD = 1);
{* 在 Win9X 下让喇叭发声}
{$ENDIF}

function GetLastErrorMsg(IncludeErrorCode: Boolean = False): string;
{* 取得最后一次错误信息}

procedure ShowLastError;
{* 显示 Win32 API 运行结果信息}

{$ENDIF}

function GetHzPy(const AHzStr: AnsiString): AnsiString;
{* 取汉字的拼音首字母，注意编码必须是 GB2312，并且只支持部分常用字
 （GBK/GB18030 编码也可但获取不到）}

{$IFDEF UNICODE}
function GetHzPyW(const AHzStr: string): string;
{* 取汉字的拼音，参数为 Utf16}
{$ENDIF}

{$IFDEF MSWINDOWS}

function TextFullWidthToHalfWidth(const Text: string): string;
{* 全角字符转换为半角字符。其中句号"。"转为"."，顿号"、"转为","}

function TextHalfWidthToFullWidth(const Text: string): string;
{* 半角字符转换为全角字符}

function GetSelText(edt: TCustomEdit): string;
{* 获得 CustomEdit 选中的字符串，可正确处理使用了 XP 样式的程序}

function SoundCardExist: Boolean;
{* 声卡是否存在}

function FindFormByClass(AClass: TClass): TForm;
{* 根据指定类名查找窗体}

function ModalFormExists: Boolean;
{* 当前是否有模态窗口存在}

{$ENDIF}

function InheritsFromClassName(ASrc: TClass; const AClass: string): Boolean; overload;
{* 判断 ASrc 是否派生自类名为 AClass 的类 }

function InheritsFromClassName(AObject: TObject; const AClass: string): Boolean; overload;
{* 判断 AObject 是否派生自类名为 AClass 的类 }

{$IFDEF MSWINDOWS}

function AdjustDebugPrivilege(Enable: Boolean): Boolean;
{* 提升自身权限到 SeDebug 或取消此权限}

{$IFNDEF WIN64}

function PEBIsDebugged: Boolean;
{* 查找 PEB 结构内容以判定本进程是否被调试中}

{$ENDIF}

function NtIsDeugged: Boolean;
{* 使用 NtQueryInformationProcess 以判定本进程是否被调试中}

procedure KillProcessByFileName(const FileName: String);
{* 根据文件名结束进程，不区分路径}

function KillProcessByFullFileName(const FullFileName: string): Boolean;
{* 根据完整文件名结束进程，区分路径，支持 32 位进程与 64 位进程交叉结束}

{$ENDIF}

function IndexStr(const AText: string; AValues: array of string; IgCase: Boolean = True): Integer;
{* 查找字符串在动态数组中的索引，用于 string 类型使用 case 语句}

function IndexInt(ANum: Integer; AValues: array of Integer): Integer;
{* 查找整形变量在动态数组中的索引，用于变量使用 case 语句}

procedure TrimStrings(AList: TStrings);
{* 删除空行和每一行的行首尾空格}

procedure RemoveDuplicatedStrings(AList: TStrings; CaseSensitive: Boolean = True); overload;
{* 字符串列表去重，CaseSensitive 表示判断重复时是否区分大小写}

procedure RemoveDuplicatedStrings(AList: TCnWideStringList; CaseSensitive: Boolean = True); overload;
{* 字符串列表去重，CaseSensitive 表示判断重复时是否区分大小写}

//==============================================================================
// 级联属性操作相关函数 by LiuXiao
//==============================================================================

function GetPropInfoIncludeSub(Instance: TObject; const PropName: string;
  AKinds: TTypeKinds = []): PPropInfo;
{* 获得级联属性信息}

function GetPropValueIncludeSub(Instance: TObject; PropName: string;
    PreferStrings: Boolean = True): Variant;
{* 获得级联属性值}

function SetPropValueIncludeSub(Instance: TObject; const PropName: string;
  const Value: Variant; AOwner: TComponent = nil): Boolean;
{* 设置级联属性值}

procedure DoSetPropValueIncludeSub(Instance: TObject; const PropName: string;
  Value: Variant; AOwner: TComponent = nil);
{* 设置级联属性值，不处理异常}

function GetOriginalClassFromProperty(AClass: TClass; const PropName: string): TClass; overload;
{* 获取某属性所在的定义的父类，无则返回 nil。
  比较 AClass 和返回值是否相等就可知道该属性是本 Class 中定义的还是父类里定义的}

function GetOriginalClassFromProperty(AObject: TObject; const PropName: string): TClass; overload;
{* 获取某属性所在的定义的父类，无则返回 nil}

function StrToSetValue(const Value: string; PInfo: PTypeInfo): Integer;
{* 字符串转集合值 }

function PropInfoName(PropInfo: PPropInfo): string;
{* 取得 PropInfo 的 Name }

function TypeInfoName(TypeInfo: PTypeInfo): string;
{* 取得 TypeInfo 的 Name }

procedure GetAllPropNames(AComp: TObject; PropNames: TStrings;
  const BaseName: string = ''; IncludeType: Boolean = False);
{* 获得某对象的所有属性的字符串值，包括子属性的属性
   IncludeType 为 True 时，格式为 Name=TypeName，Object 放入 PropType }

procedure GetAllPropNamesFromClass(ACompClass: TClass; PropNames: TStrings;
  IncludeType: Boolean = False);
{* 获得某类的所有属性的字符串值，不包括子属性的属性，因为没有实例
   IncludeType 为 True 时，格式为 Name=TypeName，Object 中放入 PropType }

//==============================================================================
// 其他杂项函数 by LiuXiao
//==============================================================================

function CnGetTickCount: Cardinal;
{* 封装的获取系统启动以来的毫秒数的函数}

{$IFDEF MSWINDOWS}

type
  TCnFontControl = class(TControl)
  public
    property ParentFont;
    property Font;
  end;

function IsParentFont(AControl: TControl): Boolean;
{* 判断某 Control 的 ParentFont 属性是否为 True，如无 Parent 则返回 False }

function GetParentFont(AControl: TComponent): TFont;
{* 取某 Control 的 Parent 的 Font 属性，如果没有返回 nil }

{$ENDIF}

type
  TCnWideMemIniFile = class(TMemIniFile)
  {* 支持宽字符串存储的 Ini 类}
  public
    constructor Create(const AFileName: string);
    procedure UpdateFile; override;
  end;

const
  InvalidFileNameChar: set of AnsiChar = ['\', '/', ':', '*', '?', '"', '<', '>', '|'];

function _CnPChar(const S: string): {$IFDEF UNICODE} PAnsiChar; inline {$ELSE} PChar {$ENDIF};
{* 封装的 PChar 转换函数，供 D2009 下与以前版本 IDE 下同时使用}

function _CnExtractFileExt(const FileName: string): string;
{* 对 ExtractFileExt 的封装，Delphi XE3 及以上的 ExtractFileExt 因为调用了 TStringHelper.LastDelimiter 导致
 ExtractFileExt('.dpr') 不再返回 '.dpr' 而是返回空值了}

function _CnExtractFileName(const FileName: string): string;
{* 对 ExtractFileName 的封装，防止 Delphi XE3 及以上的 TStringHelper.LastDelimiter 引入的不兼容}

function _CnExtractFileDir(const FileName: string): string;
{* 对 ExtractFileDir 的封装，防止 Delphi XE3 及以上的 TStringHelper.LastDelimiter 引入的不兼容}

function _CnExtractFilePath(const FileName: string): string;
{* 对 ExtractFilePath 的封装，防止 Delphi XE3 及以上的 TStringHelper.LastDelimiter 引入的不兼容}

function _CnChangeFileExt(const FileName, Extension: string): string;
{* 对 ChangeFileExt 的封装，防止 Delphi XE3 及以上的 TStringHelper.LastDelimiter 引入的不兼容}

function WideStringReplace(const S, OldPattern, NewPattern: Widestring): Widestring;
{* WideString 的全部替换实现，区分大小写}

function DoubleEqual(const D1, D2: Double): Boolean;
{* 判断浮点数是否相等（差足够小）}

function ExtendedEqual(const E1, E2: Extended): Boolean;
{* 判断浮点数是否相等（差足够小）}

function SingleEqual(const S1, S2: Single): Boolean;
{* 判断浮点数是否相等（差足够小）}

function CodePageOnlySupportsEnglish: Boolean;
{* 判断当前平台是否只支持英文，用于处理防止 Unicode -> Ansi 时丢字符的问题}

function GetSetElementCount(const ASet; ASetSize: Integer): Integer;
{* 获取某集合内的元素数目，尺寸不对则返回 -1}

{$IFDEF MSWINDOWS}

function CnMapFileToPointer(const FileName: string; out FileHandle, MapHandle: THandle;
  out Address: Pointer): Boolean;
{* 将文件映射为内存文件并返回文件句柄、映射句柄与映射的内存起始地址，成功返回 True
  如果返回 True，必须在使用完 Address 后调用 CnUnMapFileFromPointer
  以释放 Address、MapHandle 和 FileHandle}

function CnUnMapFileFromPointer(var FileHandle, MapHandle: THandle;
  var Address: Pointer): Boolean;
{* 释放映射的内存文件，与 CnMapFileToPointer 对应}

function ExtractPEDataDirectory(const FileName: string; DirectoryIndex: Integer;
  OutStream: TStream): Boolean;
{* 处理一个 PE 文件，提取出其中第 DirectoryIndex 个 DataDirectory 的内容并写到流中，返回提取是否成功}

{$ENDIF}

function LoadRawFileToBytes(const FileName: string): TBytes;
{* 从文件中载入字节数组，不进行编码转换}

procedure SaveRawFileFromBytes(B: TBytes; const FileName: string);
{* 将字节数组写入文件，不进行编码转换}

function StrToBytes(const S: AnsiString): TBytes;
{* 将 Ansi 字符串的内容转为一新的字节数组}

function BytesToStr(Data: TBytes): AnsiString;
{* 将字节数组的内容转为一新的 AnsiString}

function ConvertStringToIdent(const Str: string; const Prefix: string = 'S';
  UseUnderLine: Boolean = False; IdentWordStyle: TCnIdentWordStyle = iwsUpperFirstChar;
  UseFullPinYin: Boolean = False; MaxWideChars: Integer = 7; MaxWords: Integer = 7;
  MaxCharLength: Integer = 64): string;
{* 将字符串中的符合标识符信息的内容抽取出来形成标识符并返回，内部针对汉字处理拼音
  UseUnderLine：分词或拼音之间是否用下划线分隔（拼音使用首字母时相邻的汉字不分隔）
  IdentWordStyle：分词使用全大写、全小写还是首字母大写
  UseFullPinYin：获取汉字拼音时使用全拼还是只首字母
  MaxWideChars：最长处理的双字节字符数，要有拼音才算
  MaxWords：最长处理的分词数
  MaxCharLength: 最长的字符数。注意这三个 Max 只要达到一个就完成，可能会超 MaxCharLength}

function Int32Len(IntPtr: Pointer): Integer;
{* 返回以 0 结尾的四字节长度块的块数，不包括结尾的 0，类似于 StrLen
   注意 Int8Len 和 Int16Len 可以用 StrLen PAnsiChar/PWideChar 代替}

implementation

uses
  CnStrings, CnGB18030 {$IFDEF MSWINDOWS}, CnHexEditor {$ENDIF};

const
  MINOR_DOUBLE = 1E-8;
  MINOR_EXTENDED = 1E-10;
  MINOR_SINGLE = 1E-6;
  // 不同类型浮点数判断相等时使用的差值，依具体场合而定，尚不够准确。

{$IFNDEF DELPHIXE5_UP}
  WM_COPYGLOBALDATA = $0049; // XE4 and below does not support WM_COPYGLOBALDATA and MSGFIT_ADD
  MSGFLT_ADD        = $00000001;
{$ENDIF}

  SCnDlgButtonCaptions: array[Low(TCnDlgButtonCaption)..High(TCnDlgButtonCaption)] of PString = (
    @SCnMsgDlgCancel, @SCnMsgDlgOK, @SCnMsgDlgNo, @SCnMsgDlgYes, @SCnMsgDlgNoToAll,
    @SCnMsgDlgYesToAll
  );

  SCN_CHINESE_SEP_CHARS: array[0..11] of WideString =
    ('，', '。', '、', '！', '（', '）', '【', '】', '—', '…','《', '》');

{$IFDEF MSWINDOWS}

type
  TNtQueryInformationProcess = function(ProcessHandle: THANDLE; ProcessInformationClass: DWORD;
    ProcessInformation: Pointer; ProcessInformationLength: ULONG; ReturnLength: PULONG): LongInt; stdcall;
var
  NtQueryInformationProcess: TNtQueryInformationProcess = nil;
  NtDllHandle: THandle = 0;

{$ENDIF}

function DoubleEqual(const D1, D2: Double): Boolean;
begin
  Result := Abs(D1 - D2) < MINOR_DOUBLE;
end;

function ExtendedEqual(const E1, E2: Extended): Boolean;
begin
  Result := Abs(E1 - E2) < MINOR_EXTENDED;
end;

function SingleEqual(const S1, S2: Single): Boolean;
begin
  Result := Abs(S1 - S2) < MINOR_SINGLE;
end;

// 判断当前平台是否只支持英文，用于处理防止 Unicode -> Ansi 时丢字符的问题
function CodePageOnlySupportsEnglish: Boolean;
begin
  Result := (GetACP = 1252); // ANSI LATIN
end;

// 封装的 PChar 转换函数，供 D2009 下与以前版本 IDE 下同时使用
// 另外，D2009 或以上版本，必须加 inline，
// 避免产生实质转换时返回值指向局部被释放的内容
function _CnPChar(const S: string): {$IFDEF UNICODE} PAnsiChar; inline {$ELSE} PChar {$ENDIF};
begin
{$IFDEF UNICODE}
  Result := PAnsiChar(AnsiString(S));
{$ELSE}
  Result := PChar(S);
{$ENDIF}
end;

// 对ExtractFileExt 的封装，Delphi XE3 及以上的 ExtractFileExt 因为调用了
// TStringHelper.LastDelimiter（0 基）导致 ExtractFileExt('.dpr') 不再返回'.dpr'，
// 而是返回空值了；XE3 下 SysUtils.LastDelimiter 还是与 XE2 兼容的
function _CnExtractFileExt(const FileName: string): string;
{$IFDEF NEW_SETTING_DELIMITER}
var
  I: Integer;
begin
  I := LastDelimiter('.' + PathDelim + DriveDelim, FileName);
  if (I > 0) and (FileName[I] = '.') then
    Result := Copy(FileName, I, MaxInt) else
    Result := '';
end;
{$ELSE}
begin
  Result := ExtractFileExt(FileName);
end;
{$ENDIF}

// 对 ExtractFileName 的封装，防止 Delphi XE3 及以上的
// TStringHelper.LastDelimiter 引入的不兼容
function _CnExtractFileName(const FileName: string): string;
{$IFDEF NEW_SETTING_DELIMITER}
var
  I: Integer;
begin
  I := LastDelimiter(PathDelim + DriveDelim, FileName);
  Result := Copy(FileName, I + 1, MaxInt);
end;
{$ELSE}
begin
  Result := ExtractFileName(FileName);
end;
{$ENDIF}

// 对 ExtractFileDir 的封装，防止 Delphi XE3 及以上的 TStringHelper.LastDelimiter
// 引入的不兼容，XE3 的 ExtractFileDir('C:\1.dpr') 返回的是 'C:' 而不是 'C:\'
function _CnExtractFileDir(const FileName: string): string;
{$IFDEF NEW_SETTING_DELIMITER}
var
  I: Integer;
begin
  I := LastDelimiter(PathDelim + DriveDelim, Filename);
  if (I > 1) and (FileName[I] = PathDelim) and
    (not IsDelimiter( PathDelim + DriveDelim, FileName, I-1)) then Dec(I);
  Result := Copy(FileName, 1, I);
end;
{$ELSE}
begin
  Result := ExtractFileDir(FileName);
end;
{$ENDIF}

// 对 ExtractFilePath 的封装，防止 Delphi XE3 及以上的
// TStringHelper.LastDelimiter 引入的不兼容
function _CnExtractFilePath(const FileName: string): string;
{$IFDEF NEW_SETTING_DELIMITER}
var
  I: Integer;
begin
  I := LastDelimiter(PathDelim + DriveDelim, FileName);
  Result := Copy(FileName, 1, I);
end;
{$ELSE}
begin
  Result := ExtractFilePath(FileName);
end;
{$ENDIF}

// 对 ChangeFileExt 的封装，防止 Delphi XE3 及以上的
// TStringHelper.LastDelimiter 引入的不兼容
function _CnChangeFileExt(const FileName, Extension: string): string;
{$IFDEF NEW_SETTING_DELIMITER}
var
  I: Integer;
begin
  I := LastDelimiter('.' + PathDelim + DriveDelim, Filename);
  if (I = 0) or (FileName[I] <> '.') then I := MaxInt;
  Result := Copy(FileName, 1, I - 1) + Extension;
end;
{$ELSE}
begin
  Result := ChangeFileExt(FileName, Extension);
end;
{$ENDIF}

//==============================================================================
// Ansi 字符串函数
//==============================================================================

// WideString 的全部替换实现，区分大小写
function WideStringReplace(const S, OldPattern, NewPattern: WideString): WideString;
var
  SearchStr, Patt, NewStr: WideString;
  Offset: Integer;
begin
  SearchStr := S;
  Patt := OldPattern;

  NewStr := S;
  Result := '';
  while SearchStr <> '' do
  begin
    Offset := Pos(Patt, SearchStr);
    if Offset = 0 then
    begin
      Result := Result + NewStr;
      Break;
    end;
    Result := Result + Copy(NewStr, 1, Offset - 1) + NewPattern;
    NewStr := Copy(NewStr, Offset + Length(OldPattern), MaxInt);

    SearchStr := Copy(SearchStr, Offset + Length(Patt), MaxInt);
  end;
end;

function AnsiTrim(const S: AnsiString): AnsiString;
var
  I, L: Integer;
begin
  L := Length(S);
  I := 1;
  while (I <= L) and (S[I] <= ' ') do Inc(I);
  if I > L then Result := '' else
  begin
    while S[L] <= ' ' do Dec(L);
    Result := Copy(S, I, L - I + 1);
  end;
end;

function TrimBom(const S: AnsiString): AnsiString;
const
  UTF8_BOM = #$EF#$BB#$BF;
begin
  if Length(S) < Length(UTF8_BOM) then
    Result := S
  else if StrLComp(PAnsiChar(UTF8_BOM), PAnsiChar(S), Length(UTF8_BOM)) = 0 then
    Result := Copy(S, Length(UTF8_BOM) + 1, MaxInt)
  else
    Result := S;
end;

//------------------------------------------------------------------------------
// 扩展的文件目录操作函数
//------------------------------------------------------------------------------

{$IFDEF MSWINDOWS}

// 在资源管理器中打开指定目录
procedure ExploreDir(const APath: string; ShowDir: Boolean);
var
  strExecute: AnsiString;
begin
  if not ShowDir then
    strExecute := AnsiString(Format('EXPLORER.EXE "%s"', [APath]))
  else
    strExecute := AnsiString(Format('EXPLORER.EXE /e, "%s"', [APath]));
  WinExec(PAnsiChar(strExecute), SW_SHOWNORMAL);
end;

// 在资源管理器中打开指定文件
procedure ExploreFile(const AFile: string; ShowDir: Boolean);
var
  strExecute: AnsiString;
begin
  if not ShowDir then
    strExecute := AnsiString(Format('EXPLORER.EXE /select, "%s"', [AFile]))
  else
    strExecute := AnsiString(Format('EXPLORER.EXE /e, /select, "%s"', [AFile]));
  WinExec(PAnsiChar(strExecute), SW_SHOWNORMAL);
end;

// 递归创建多级子目录
function ForceDirectories(Dir: string): Boolean;
begin
  Result := True;

  if Length(Dir) = 0 then
  begin
    Result := False;
    Exit;
  end;
  Dir := ExcludeTrailingBackslash(Dir);
  if (Length(Dir) < 3) or DirectoryExists(Dir)
    or (_CnExtractFilePath(Dir) = Dir) then
    Exit;                                // avoid 'xyz:\' problem.
  Result := ForceDirectories(_CnExtractFilePath(Dir)) and CreateDir(Dir);
end;

// 移动文件、目录
function MoveFile(const sName, dName: string): Boolean;
var
  S1, S2: string;
  lpFileOp: TSHFileOpStruct;
begin
  S1 := PChar(sName) + #0#0;
  S2 := PChar(dName) + #0#0;
  with lpFileOp do
  begin
    Wnd := Application.Handle;
    wFunc := FO_MOVE;
    pFrom := PChar(S1);
    pTo := PChar(S2);
    fFlags := FOF_ALLOWUNDO;
    hNameMappings := nil;
    lpszProgressTitle := nil;
    fAnyOperationsAborted := True;
  end;

  try
    Result := SHFileOperation(lpFileOp) = 0;
  except
    Result := False;
  end;
end;

// 删除文件到回收站
function DeleteToRecycleBin(const FileName: string): Boolean;
var
  S: string;
  lpFileOp: TSHFileOpStruct;
begin
  S := PChar(FileName) + #0#0;
  with lpFileOp do
  begin
    Wnd := Application.Handle;
    wFunc := FO_DELETE;
    pFrom := PChar(S);
    pTo := nil;
    fFlags := FOF_ALLOWUNDO or FOF_SILENT or FOF_NOCONFIRMATION;
    hNameMappings := nil;
    lpszProgressTitle := nil;
    fAnyOperationsAborted := True;
  end;

  try
    Result := SHFileOperation(lpFileOp) = 0;
  except
    Result := False;
  end;
end;

// 打开文件属性窗口
procedure FileProperties(const FName: string);
var
  SEI: SHELLEXECUTEINFO;
begin
  with SEI do
  begin
    cbSize := SizeOf(SEI);
    fMask := SEE_MASK_NOCLOSEPROCESS or SEE_MASK_INVOKEIDLIST or
      SEE_MASK_FLAG_NO_UI;
    Wnd := Application.Handle;
    lpVerb := 'properties';
    lpFile := PChar(FName);
    lpParameters := nil;
    lpDirectory := nil;
    nShow := 0;
    hInstApp := 0;
    lpIDList := nil;
  end;
{$IFDEF FPC}
  ShellExecuteEx(LPSHELLEXECUTEINFO(@SEI));
{$ELSE}
  ShellExecuteEx(@SEI);
{$ENDIF}
end;

// 缩短显示不下的长路径名
function FormatPath(const APath: string; Width: Integer): string;
var
  SLen: Integer;
  I, J: Integer;
  TString: string;
begin
  SLen := Length(APath);
  if (SLen <= Width) or (Width <= 6) then
  begin
    Result := APath;
    Exit
  end
  else
  begin
    I := SLen;
    TString := APath;
    for J := 1 to 2 do
    begin
      while (TString[I] <> '\') and (SLen - I < Width - 8) do
        I := I - 1;
      I := I - 1;
    end;
    for J := SLen - I - 1 downto 0 do
      TString[Width - J] := TString[SLen - J];
    for J := SLen - I to SLen - I + 2 do
      TString[Width - J] := '.';
    Delete(TString, Width + 1, 255);
    Result := TString;
  end;
end;

// 通过 DrawText 来画缩略路径
procedure DrawCompactPath(Hdc: HDC; Rect: TRect; const Str: string);
begin
  DrawText(Hdc, PChar(Str), Length(Str), Rect, DT_PATH_ELLIPSIS);
end;

// 在指定 Canvas 上绘制匹配的字符串，匹配部分高亮显示
procedure DrawMatchText(Canvas: TCanvas; const MatchStr, Text: string;
  X, Y: Integer; HighlightColor: TColor; MatchedIndexes: TList; StartOffset: Integer);
var
  MatchIdx, I, W, L: Integer;
  HdrStr, AMatchStr, TailStr, PaintStr: string;
  OldColor, OldBrushColor: TColor;
  OldStyle: TBrushStyle;
  ASize: TSize;
  C: Char;
begin
  OldStyle := Canvas.Brush.Style;
  OldBrushColor := Canvas.Brush.Color;
  Canvas.Brush.Style := bsClear;

  // 所有文字均采用 bsClear 模式绘制
  if (MatchedIndexes = nil) or (MatchedIndexes.Count = 0) then
  begin
    if MatchStr = '' then
      MatchIdx := 0
    else if StartOffset > 1 then
    begin
      TailStr := Copy(Text, StartOffset, MaxInt);
      MatchIdx := Pos(UpperCase(Trim(MatchStr)), UpperCase(TailStr));
      Inc(MatchIdx, StartOffset - 1);
    end
    else
      MatchIdx := Pos(UpperCase(Trim(MatchStr)), UpperCase(Text));

    if MatchIdx > 0 then
    begin
      HdrStr := Copy(Text, 1, MatchIdx - 1);
      AMatchStr := Copy(Text, MatchIdx, Length(Trim(MatchStr)));
      TailStr := Copy(Text, MatchIdx + Length(Trim(MatchStr)), MaxInt);

      Canvas.TextOut(X, Y, HdrStr);
      Inc(X, Canvas.TextWidth(HdrStr));
      OldColor := Canvas.Font.Color;
      Canvas.Font.Color := HighlightColor;
      Canvas.TextOut(X, Y, AMatchStr);
      Canvas.Font.Color := OldColor;
      Inc(X, Canvas.TextWidth(AMatchStr));
      Canvas.TextOut(X, Y, TailStr);
    end
    else
      Canvas.TextOut(X, Y, Text);

    Canvas.Brush.Style := OldStyle;
  end
  else
  begin
    Canvas.TextOut(X, Y, Text);
    SetLength(PaintStr, Length(Text));
    StrCopy(PChar(PaintStr), PChar(Text));
    OldColor := Canvas.Font.Color;
    Canvas.Font.Color := HighlightColor;

    for I := MatchedIndexes.Count - 1 downto 0 do
    begin
      L := Integer(MatchedIndexes[I]);
      if (L <= 0) or (L > Length(PaintStr)) then
        Continue;

      if L < Length(PaintStr) then
        PaintStr[L + 1] := #0;
      C := PaintStr[L];
      PaintStr[L] := #0;

      ASize.cx := 0;
      ASize.cy := 0;
      if L = 1 then
        W := 0
      else
      begin
        Windows.GetTextExtentPoint32(Canvas.Handle, PChar(@(PaintStr[1])), L - 1, ASize);
        W := ASize.cx; // 计算需绘制字符前的宽度
      end;
      PaintStr[L] := C;
      Windows.TextOut(Canvas.Handle, X + W, Y, PChar(@(PaintStr[L])), 1);
    end;
    SetLength(PaintStr, 0);
    Canvas.Font.Color := OldColor;
  end;
  Canvas.Brush.Style := OldStyle;
  Canvas.Brush.Color := OldBrushColor;
end;

// 打开文件框
function OpenDialog(var FileName: string; const Title: string; const Filter: string;
  const Ext: string): Boolean;
var
  OpenName: TOPENFILENAME;
  TempFilename, ReturnFile: string;
begin
  with OpenName do
  begin
    lStructSize := SizeOf(OpenName);
    hWndOwner := GetModuleHandle('');
{$IFDEF FPC}
    Hinstance := GetModuleHandle(nil);
{$ELSE}
    Hinstance := SysInit.Hinstance;
{$ENDIF}
    lpstrFilter := PChar(Filter + #0 + Ext + #0#0);
    lpstrCustomFilter := '';
    nMaxCustFilter := 0;
    nFilterIndex := 1;
    nMaxFile := MAX_PATH;
    SetLength(TempFilename, nMaxFile + 2);
    lpstrFile := PChar(TempFilename);
    FillChar(lpstrFile^, MAX_PATH, 0);
    SetLength(TempFilename, nMaxFile + 2);
    nMaxFileTitle := MAX_PATH;
    SetLength(ReturnFile, MAX_PATH + 2);
    lpstrFileTitle := PChar(ReturnFile);
    FillChar(lpstrFile^, MAX_PATH, 0);
    lpstrInitialDir := '.';
    lpstrTitle := PChar(Title);
    Flags := OFN_HIDEREADONLY + OFN_ENABLESIZING;
    nFileOffset := 0;
    nFileExtension := 0;
    lpstrDefExt := PChar(Ext);
    lCustData := 0;
    lpfnHook := nil;
    lpTemplateName := '';
  end;

{$IFDEF FPC}
  Result := GetOpenFileName(LPOPENFILENAME(@OpenName));
{$ELSE}
  Result := GetOpenFileName(OpenName);
{$ENDIF}

  if Result then
    FileName := ReturnFile
  else
    FileName := '';
end;

function SelectDirCB(Wnd: HWND; uMsg: UINT; lParam, lpData: LPARAM): Integer stdcall;
begin
  if (uMsg = BFFM_INITIALIZED) and (lpData <> 0) then
    SendMessage(Wnd, BFFM_SETSELECTION, Integer(True), lpdata);
  Result := 0;
end;

function CnSelectDirectory(const Caption: string; const Root: WideString;
  var Directory: string; Owner: HWND; ShowNewButton: Boolean = True): Boolean;
var
  BrowseInfo: TBrowseInfo;
  Buffer: PChar;
  RootItemIDList, ItemIDList: PItemIDList;
  ShellMalloc: IMalloc;
  IDesktopFolder: IShellFolder;
  Eaten, Flags: LongWord;
begin
  Result := False;
  FillChar(BrowseInfo, SizeOf(BrowseInfo), 0);
  if (ShGetMalloc(ShellMalloc) = S_OK) and (ShellMalloc <> nil) then
  begin
    Buffer := ShellMalloc.Alloc(MAX_PATH);
    try
      SHGetDesktopFolder(IDesktopFolder);
      if Root = '' then
        RootItemIDList := nil
      else
        IDesktopFolder.ParseDisplayName(Application.Handle, nil,
          POleStr(Root), Eaten, RootItemIDList, Flags);
      with BrowseInfo do
      begin
        hwndOwner := Owner;
        pidlRoot := RootItemIDList;
        pszDisplayName := Buffer;
        lpszTitle := PChar(Caption);
        ulFlags := BIF_RETURNONLYFSDIRS;
        if ShowNewButton then
          ulFlags := ulFlags or $0040;
        lpfn := SelectDirCB;
        lparam := TCnNativeUInt(PChar(Directory));
      end;
      ItemIDList := SHBrowseForFolder(BrowseInfo);
      Result :=  ItemIDList <> nil;
      if Result then
      begin
        ShGetPathFromIDList(ItemIDList, Buffer);
        ShellMalloc.Free(ItemIDList);
        Directory := Buffer;
      end;
    finally
      ShellMalloc.Free(Buffer);
    end;
  end;
end;

function GetDirectory(const Caption: string; var Dir: string;
  ShowNewButton: Boolean): Boolean;
var
  OldErrorMode: UINT;
  BrowseRoot: WideString;
  OwnerHandle: HWND;
begin
  OldErrorMode := SetErrorMode(SEM_FAILCRITICALERRORS);
  try
    BrowseRoot := '';
    if Screen.ActiveCustomForm <> nil then
      OwnerHandle := Screen.ActiveCustomForm.Handle
    else
      OwnerHandle := Application.Handle;
    Result := CnSelectDirectory(Caption, BrowseRoot, Dir, OwnerHandle,
      ShowNewButton);
  finally
    SetErrorMode(OldErrorMode);
  end;
end;

{*****************************************************}
{ SelectDirectoryW 函数 类似 SelectDirectory 函数     }
{ 但你可以定义初始目录 ,并且它支持 UniCode 字符       }
{ author: Arthur(阿胜), yzdbs@msn.com, 08.06.16       }
{ hOwn: 父窗口句柄                                    }
{ Path: 目录输入输出参数, 输入时为初始目录            }
{ Caption: 提示文本                                   }
{ Root: 根目录                                        }
{ uFlag: 其他样式，你可以这样使用,像                  }
{       BIF_RETURNONLYFSDIRS or BIF_VALIDATE          }
{       请参照 Win32SDK 查看更多明细                  }
{*****************************************************}
function SelectDirectoryW(hOwn: HWND; var Path: WideString; const Caption,
  Root: WideString; uFlag: DWORD = $25): Boolean;
const
  BIF_NEWDIALOGSTYLE = $0040;
{$IFDEF ver185}
{$REGION ' 对话框样式常量'}
(*
{ Browsing for directory. }

  {$EXTERNALSYM BIF_RETURNONLYFSDIRS}
  BIF_RETURNONLYFSDIRS   = $0001;  { For finding a folder to start document searching }
  {$EXTERNALSYM BIF_DONTGOBELOWDOMAIN}
  BIF_DONTGOBELOWDOMAIN  = $0002;  { For starting the Find Computer }
  {$EXTERNALSYM BIF_STATUSTEXT}
  BIF_STATUSTEXT         = $0004;
  {$EXTERNALSYM BIF_RETURNFSANCESTORS}
  BIF_RETURNFSANCESTORS  = $0008;
  {$EXTERNALSYM BIF_EDITBOX}
  BIF_EDITBOX            = $0010;
  {$EXTERNALSYM BIF_VALIDATE}
  BIF_VALIDATE           = $0020;  { insist on valid result (or CANCEL) }
  {$EXTERNALSYM BIF_NEWDIALOGSTYLE}
  BIF_NEWDIALOGSTYLE     = $0040;
  {$EXTERNALSYM BIF_USENEWUI}
  BIF_USENEWUI = BIF_NEWDIALOGSTYLE or BIF_EDITBOX;

  {$EXTERNALSYM BIF_BROWSEINCLUDEURLS}
  BIF_BROWSEINCLUDEURLS  = $0080;
  {$EXTERNALSYM BIF_UAHINT}
  BIF_UAHINT = $100;   // Add a UA hint to the dialog, in place of the edit box. May not be combined with BIF_EDITBOX
  {$EXTERNALSYM BIF_NONEWFOLDERBUTTON}
  BIF_NONEWFOLDERBUTTON = $200;   // Do not add the "New Folder" button to the dialog.  Only applicable with BIF_NEWDIALOGSTYLE.
  {$EXTERNALSYM BIF_NOTRANSLATETARGETS}
  BIF_NOTRANSLATETARGETS = $400;   // don't traverse target as shortcut

  {$EXTERNALSYM BIF_BROWSEFORCOMPUTER}
  BIF_BROWSEFORCOMPUTER  = $1000;  { Browsing for Computers. }
  {$EXTERNALSYM BIF_BROWSEFORPRINTER}
  BIF_BROWSEFORPRINTER   = $2000;  { Browsing for Printers }
  {$EXTERNALSYM BIF_BROWSEINCLUDEFILES}
  BIF_BROWSEINCLUDEFILES = $4000;  { Browsing for Everything }
  {$EXTERNALSYM BIF_SHAREABLE}
  BIF_SHAREABLE          = $8000;
*)
{$ENDREGION}
{$ENDIF}
var
  BrowseInfo: TBrowseInfoW;
  Buffer: PWideChar;
  RootItemIDList, ItemIDList: PItemIDList;
  ShellMalloc: IMalloc;
  IDesktopFolder: IShellFolder;
  Dummy: LongWord;

  function BrowseCallbackProc(hwnd: HWND; uMsg: UINT; lParam: Cardinal;
    lpData: Cardinal): Integer; stdcall;
  var
    PathName: array[0..MAX_PATH] of WideChar;
  begin
    case uMsg of
      BFFM_INITIALIZED:
        SendMessage(Hwnd, BFFM_SETSELECTION, Ord(True), TCnNativeUInt(lpData));
      BFFM_SELCHANGED:
        begin
          SHGetPathFromIDListW(PItemIDList(lParam), @PathName);
          SendMessage(hwnd, BFFM_SETSTATUSTEXTW, 0,
            LongInt(PWideChar(WideString(
            @PathName))));
        end;
    end;
    Result := 0;
  end;
begin
  Result := False;
  FillChar(BrowseInfo, SizeOf(BrowseInfo), 0);
  if (ShGetMalloc(ShellMalloc) = S_OK) and (ShellMalloc <> nil) then
  begin
    Buffer := ShellMalloc.Alloc(MAX_PATH);
    try
      RootItemIDList := nil;
      if Root <> '' then
      begin
        SHGetDesktopFolder(IDesktopFolder);
        IDesktopFolder.ParseDisplayName(hOwn, nil, POleStr(WideString(Root)),
          Dummy, RootItemIDList, Dummy);
      end;
      with BrowseInfo do
      begin
        hwndOwner := hOwn;
        pidlRoot := RootItemIDList;
        pszDisplayName := Buffer;
        lpszTitle := PWideChar(Caption);
        ulFlags := uFlag;
        lpfn := @BrowseCallbackProc;
        lParam := TCnNativeUInt(PWideChar(Path));
      end;
      ItemIDList := SHBrowseForFolderW(BrowseInfo);
      Result := ItemIDList <> nil;
      if Result then
      begin
        SHGetPathFromIDListW(ItemIDList, Buffer);
        ShellMalloc.Free(ItemIDList);
        Path := WideString(Buffer);
      end;
    finally
      ShellMalloc.Free(Buffer);
    end;
  end;
end;

// 对 SelectDirectoryW 的封装
function SelectDirectoryEx(hOwn: HWND; const Caption, Root: WideString): WideString;
begin
  SelectDirectoryW(hOwn, Result, Caption, Root);
end;

{$ENDIF}

// 两个字符串的前面的相同字符数
function SameCharCounts(S1, S2: string): Integer;
var
  Str1, Str2: PChar;
begin
  Result := 1;
  S1 := S1 + #0;
  S2 := S2 + #0;
  Str1 := PChar(S1);
  Str2 := PChar(S2);

  while (S1[Result] = S2[Result]) and (S1[Result] <> #0) do
  begin
    Inc(Result);
  end;
  Dec(Result);
{$IFDEF MSWINDOWS}
  if (StrByteType(Str1, Result - 1) = mbLeadByte) or
    (StrByteType(Str2, Result - 1) = mbLeadByte) then
    Dec(Result);
{$ENDIF}
{$IFDEF POSIX}
  if (StrByteType(Str1, Result - 1) <> mbSingleByte) or
    (StrByteType(Str2, Result - 1) <> mbSingleByte) then
    Dec(Result);
{$ENDIF}
end;

// 在字符串中某字符出现的次数
function CharCounts(Str: PChar; Chr: Char): Integer;
var
  P: PChar;
begin
  Result := 0;
  P := StrScan(Str, Chr);
  while P <> nil do
  begin
{$IFDEF MSWINDOWS}
    case StrByteType(Str, Integer(P - Str)) of
      mbSingleByte: begin
        Inc(Result);
        Inc(P);
      end;
      mbLeadByte: Inc(P);
    end;
{$ENDIF}
{$IFDEF POSIX}
    if StrByteType(Str, Integer(P - Str)) = mbSingleByte then
    begin
      Inc(Result);
      Inc(P);
    end;
{$ENDIF}
    Inc(P);
    P := StrScan(P, Chr);
  end;
end;

// 取两个目录的相对路径
function GetRelativePath(ATo, AFrom: string;
  const PathStr: string = '\'; const ParentStr: string = '..';
  const CurrentStr: string = '.'; const UseCurrentDir: Boolean = False): string;
var
  I, HeadNum: Integer;
begin
  ATo := StringReplace(ATo, '/', '\', [rfReplaceAll]);
  AFrom := StringReplace(AFrom, '/', '\', [rfReplaceAll]);
  while AnsiPos('\\', ATo) > 0 do
    ATo := StringReplace(ATo, '\\', '\', [rfReplaceAll]);
  while AnsiPos('\\', AFrom) > 0 do
    AFrom := StringReplace(AFrom, '\\', '\', [rfReplaceAll]);
  if StrRight(ATo, 1) = ':' then
    ATo := ATo + '\';
  if StrRight(AFrom, 1) = ':' then
    AFrom := AFrom + '\';

  HeadNum := SameCharCounts(AnsiUpperCase(_CnExtractFilePath(ATo)),
    AnsiUpperCase(_CnExtractFilePath(AFrom)));

  // HeadNum 表示俩目录名前面相同的部分，注意可能最后相同的部分是前缀相同的不同目录名，
  // 因此，如果末尾都不是\，则需要往回找到都是 \ 的位置
  while HeadNum > 0 do
  begin
    if (ATo[HeadNum] = '\') and (AFrom[HeadNum] = '\') then
      Break;
    Dec(HeadNum);
  end;

  if HeadNum > 0 then
  begin
    ATo := StringReplace(Copy(ATo, HeadNum + 1, MaxInt), '\', PathStr, [rfReplaceAll]);
    AFrom := Copy(AFrom, HeadNum + 1, MaxInt);

    Result := '';
    HeadNum := CharCounts(PChar(AFrom), '\');
    for I := 1 to HeadNum do
      Result := Result + ParentStr + PathStr;
    if (Result = '') and UseCurrentDir then
      Result := CurrentStr + PathStr;
    Result := Result + ATo;
  end
  else
    Result := ATo;
end;

// 寻找一批文件的共同根路径，并将去除共同根路径的文件名放至 OutFiles 中，返回是否去除成功
function CombineCommonPath(InFiles, OutFiles: TStrings): Boolean;
var
  I, SC: Integer;
  Root, S: string;
begin
  Result := False;
  if (InFiles <> nil) and (OutFiles <> nil) and (InFiles.Count > 1) then
  begin
    Root := InFiles[0];
    for I := 1 to InFiles.Count - 1 do
    begin
      SC := SameCharCounts(Root, InFiles[I]);
      if SC <= 0 then
        Exit;

      if SC < Length(Root) then
      begin
        Root := Copy(Root, 1, SC);
        SC := LastDelimiter('\', Root);
        if SC > 0 then
          Root := Copy(Root, 1, SC);  // Root 是公共路径
      end;

      if Root = '' then
        Break;
    end;

    SC := Length(Root);
    Result := SC > 0;
    if Result then
    begin
      OutFiles.Clear;
      for I := 0 to InFiles.Count - 1 do
      begin
        S := InFiles[I];
        Delete(S, 1, SC);
        OutFiles.Add(S);
      end;
    end;
  end;
end;

{$IFNDEF BCB}
const
  shlwapi32 = 'shlwapi.dll';

function PathRelativePathToA; external shlwapi32 name 'PathRelativePathToA';
function PathRelativePathToW; external shlwapi32 name 'PathRelativePathToW';
function PathRelativePathTo; external shlwapi32 name 'PathRelativePathToA';

// 使用 Windows API 取两个目录的相对路径
function RelativePath(const AFrom, ATo: string; FromIsDir, ToIsDir: Boolean): string;
  function GetAttr(IsDir: Boolean): DWORD;
  begin
    if IsDir then
      Result := FILE_ATTRIBUTE_DIRECTORY
    else
      Result := FILE_ATTRIBUTE_NORMAL;
  end;
var
  p: array[0..MAX_PATH] of Char;
begin
  PathRelativePathTo(p, PChar(AFrom), GetAttr(FromIsDir), PChar(ATo), GetAttr(ToIsDir));
  Result := StrPas(p);
end;
{$ENDIF}

// 连接两个路径，
// Head - 首路径，可以是 C:\Test、\\Test\C\Abc、http://www.abc.com/dir/ 等格式
// Tail - 尾路径，可以是 ..\Test、Abc\Temp、\Test、/web/lib 等格式或绝对地址格式
function LinkPath(const Head, Tail: string): string;
var
  HeadIsUrl: Boolean;
  TailHasRoot: Boolean;
  TailIsRel: Boolean;
  AHead, ATail, S: string;
  UrlPos, I: Integer;
begin
  if Head = '' then
  begin
    Result := Tail;
    Exit;
  end;

  if Tail = '' then
  begin
    Result := Head;
    Exit;
  end;

  TailHasRoot := (AnsiPos(':\', Tail) = 2) or // C:\Test
                 (AnsiPos('\\', Tail) = 1) or // \\Name\C\Test
                 (AnsiPos('://', Tail) > 0);  // ftp://ftp.abc.com
  if TailHasRoot then
  begin
    Result := Tail;
    Exit;
  end;

  UrlPos := AnsiPos('://', Head);
  HeadIsUrl := UrlPos > 0;
  AHead := StringReplace(Head, '/', '\', [rfReplaceAll]);
  ATail := StringReplace(Tail, '/', '\', [rfReplaceAll]);

  TailIsRel := ATail[1] = '\'; // 尾路径是相对路径
  if TailIsRel then
  begin
    if AnsiPos(':\', AHead) = 2 then
      Result := AHead[1] + ':' + ATail
    else if AnsiPos('\\', AHead) = 1 then
    begin
      S := Copy(AHead, 3, MaxInt);
      I := AnsiPos('\', S);
      if I > 0 then
        Result := Copy(AHead, 1, I + 1) + ATail
      else
        Result := AHead + ATail;
    end else if HeadIsUrl then
    begin
      S := Copy(AHead, UrlPos + 3, MaxInt);
      I := AnsiPos('\', S);
      if I > 0 then
        Result := Copy(AHead, 1, I + UrlPos + 1) + ATail
      else
        Result := AHead + ATail;
    end
    else
    begin
      Result := Tail;
      Exit;
    end;
  end
  else
  begin
    if Copy(ATail, 1, 2) = '.\' then
      Delete(ATail, 1, 2);
    AHead := MakeDir(AHead);
    I := Pos('..\', ATail);
    while I > 0 do
    begin
      AHead := _CnExtractFileDir(AHead);
      Delete(ATail, 1, 3);
      I := Pos('..\', ATail);
    end;
    Result := MakePath(AHead) + ATail;
  end;

  if HeadIsUrl then
    Result := StringReplace(Result, '\', '/', [rfReplaceAll]);
end;

{$IFDEF MSWINDOWS}

// 运行一个文件
procedure RunFile(const FName: string; Handle: THandle;
  const Param: string);
begin
  ShellExecute(Handle, nil, PChar(FName), PChar(Param), nil, SW_SHOWNORMAL);
end;

// 打开一个链接
procedure OpenUrl(const Url: string; UseCmd: Boolean);
begin
  // Do not check protocal prefix.
  if CheckWindows9598 or not UseCmd then
    RunFile(Url)
  else
    ShellExecute(0, 'open', 'cmd.exe', PChar('/c start ' + Url), '', SW_HIDE);
end;

// 发送邮件
procedure MailTo(const Addr: string; const Subject: string; UseCmd: Boolean);
const
  csPrefix = 'mailto:';
  csSubject = '?Subject=';
var
  Url: string;
begin
  if Pos(csPrefix, Addr) < 1 then
    Url := csPrefix + Addr
  else
    Url := Addr;
  if Subject <> '' then
    Url := Url + csSubject + Subject;

  if CheckWindows9598 or not UseCmd then
    RunFile(Url)
  else
    ShellExecute(0, 'open', 'cmd.exe', PChar('/c start ' + Url), '', SW_HIDE);
end;

procedure SendMailTo(const Subject, Body, Addr: string; Attachs: array of string);
var
  MM, MS: Variant;
  I: Integer;
begin
  MS := CreateOleObject('MSMAPI.MAPISession');
  try
    MM := CreateOleObject('MSMAPI.MAPIMessages');
    try
      MS.DownLoadMail := False;
      MS.NewSession := False;
      MS.LogonUI := True;
      MS.SignOn;
      MM.SessionID := MS.SessionID;

      MM.Compose;

      MM.RecipIndex := 0;
      MM.RecipAddress := Addr;
      MM.MsgSubject := Subject;
      MM.MsgNoteText := Body;

      for I := Low(Attachs) to High(Attachs) do
      begin
        MM.AttachmentIndex := I;
        MM.AttachmentPathName := Attachs[I];
      end;
      MM.Send(True);
      MS.SignOff;
    finally
{$IFDEF FPC}
      Finalize(MS);
{$ELSE}
      VarClear(MS);
{$ENDIF}
    end;
  finally
{$IFDEF FPC}
    Finalize(MM);
{$ELSE}
    VarClear(MM);
{$ENDIF}
  end;
end;

// 运行一个文件并立即返回
function WinExecute(const FileName: string; Visibility: Integer = SW_NORMAL): Boolean;
var
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
  CmdLines: array[0..512] of Char;
begin
  FillChar(StartupInfo, SizeOf(StartupInfo), #0);
  StartupInfo.cb := SizeOf(StartupInfo);
  StartupInfo.dwFlags := STARTF_USESHOWWINDOW;
  StartupInfo.wShowWindow := Visibility;
  StrPLCopy(@CmdLines[0], FileName, SizeOf(CmdLines) - 1);
  Result := CreateProcess(nil, PChar(@CmdLines[0]), nil, nil, False,
    CREATE_NEW_CONSOLE or NORMAL_PRIORITY_CLASS, nil, nil, StartupInfo,
    ProcessInfo);
end;

// 运行一个文件并等待其结束
function WinExecAndWait32(const FileName: string; Visibility: Integer;
  ProcessMsg: Boolean): Integer;
var
  zAppName: array[0..512] of Char;
  zCurDir: array[0..255] of Char;
  WorkDir: string;
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
begin
  StrPLCopy(zAppName, FileName, SizeOf(zAppName) - 1);
  GetDir(0, WorkDir);
  StrPCopy(zCurDir, WorkDir);
  FillChar(StartupInfo, SizeOf(StartupInfo), #0);
  StartupInfo.cb := SizeOf(StartupInfo);

  StartupInfo.dwFlags := STARTF_USESHOWWINDOW;
  StartupInfo.wShowWindow := Visibility;
  if not CreateProcess(nil,
    zAppName,                           { pointer to command line string }
    nil,                                { pointer to process security attributes }
    nil,                                { pointer to thread security attributes }
    False,                              { handle inheritance flag }
    CREATE_NEW_CONSOLE or               { creation flags }
    NORMAL_PRIORITY_CLASS,
    nil,                                { pointer to new environment block }
    nil,                                { pointer to current directory name }
    StartupInfo,                        { pointer to STARTUPINFO }
    ProcessInfo) then
    Result := -1                        { pointer to PROCESS_INF }
  else
  begin
    if ProcessMsg then
    begin
      repeat
        Application.ProcessMessages;
        GetExitCodeProcess(ProcessInfo.hProcess, Cardinal(Result));
      until (Result <> STILL_ACTIVE) or Application.Terminated;
    end
    else
    begin
      WaitforSingleObject(ProcessInfo.hProcess, INFINITE);
      GetExitCodeProcess(ProcessInfo.hProcess, Cardinal(Result));
    end;
  end;
end;

// 用管道方式在 Dir 目录执行 CmdLine，Output 返回输出信息，
// dwExitCode 返回退出码。如果成功返回 True
function WinExecWithPipe(const CmdLine, Dir: string; slOutput: TStrings;
  var dwExitCode: Cardinal): Boolean;
var
  HOutRead, HOutWrite: THandle;
  StartInfo: TStartupInfo;
  ProceInfo: TProcessInformation;
  sa: TSecurityAttributes;
  InStream: THandleStream;
  strTemp: string;
  PDir: PChar;
  CmdLines: array[0..512] of Char;

  procedure ReadLinesFromPipe(IsEnd: Boolean);
  var
    S: AnsiString;
    ls: TStringList;
    I: Integer;
  begin
    if InStream.Position < InStream.Size then
    begin
      SetLength(S, InStream.Size - InStream.Position);
      InStream.Read(PAnsiChar(S)^, InStream.Size - InStream.Position);
      strTemp := strTemp + string(S);
      ls := TStringList.Create;
      try
        ls.Text := strTemp;
        for I := 0 to ls.Count - 2 do
          slOutput.Add(ls[I]);
        strTemp := ls[ls.Count - 1];
      finally
        ls.Free;
      end;
    end;

    if IsEnd and (strTemp <> '') then
    begin
      slOutput.Add(strTemp);
      strTemp := '';
    end;
  end;

begin
  dwExitCode := 0;
  Result := False;
  try
    FillChar(sa, sizeof(sa), 0);
    sa.nLength := sizeof(sa);
    sa.bInheritHandle := True;
    sa.lpSecurityDescriptor := nil;
    InStream := nil;
    strTemp := '';
    HOutRead := INVALID_HANDLE_VALUE;
    HOutWrite := INVALID_HANDLE_VALUE;
    try
      Win32Check(CreatePipe(HOutRead, HOutWrite, @sa, 0));

      FillChar(StartInfo, SizeOf(StartInfo), 0);
      StartInfo.cb := SizeOf(StartInfo);
      StartInfo.wShowWindow := SW_HIDE;
      StartInfo.dwFlags := STARTF_USESTDHANDLES + STARTF_USESHOWWINDOW;
      StartInfo.hStdError := HOutWrite;
      StartInfo.hStdInput := GetStdHandle(STD_INPUT_HANDLE);
      StartInfo.hStdOutput := HOutWrite;

      InStream := THandleStream.Create(HOutRead);

      if Dir <> '' then
        PDir := PChar(Dir)
      else
        PDir := nil;

      StrPLCopy(@CmdLines[0], CmdLine, SizeOf(CmdLines) - 1);
      Win32Check(CreateProcess(nil, //lpApplicationName: PChar
        PChar(@CmdLines[0]), //lpCommandLine: PChar
        nil, //lpProcessAttributes: PSecurityAttributes
        nil, //lpThreadAttributes: PSecurityAttributes
        True, //bInheritHandles: BOOL
        NORMAL_PRIORITY_CLASS, //CREATE_NEW_CONSOLE,
        nil,
        PDir,
        StartInfo,
        ProceInfo));

      while WaitForSingleObject(ProceInfo.hProcess, 100) = WAIT_TIMEOUT do
      begin
        ReadLinesFromPipe(False);
        Application.ProcessMessages;
        //if Application.Terminated then break;
      end;
      ReadLinesFromPipe(True);

      GetExitCodeProcess(ProceInfo.hProcess, dwExitCode);

      CloseHandle(ProceInfo.hProcess);
      CloseHandle(ProceInfo.hThread);

      Result := True;
    finally
      if InStream <> nil then InStream.Free;
      if HOutRead <> INVALID_HANDLE_VALUE then CloseHandle(HOutRead);
      if HOutWrite <> INVALID_HANDLE_VALUE then CloseHandle(HOutWrite);
    end;
  except
    ;
  end;
end;

function WinExecWithPipe(const CmdLine, Dir: string; var Output: string;
  var dwExitCode: Cardinal): Boolean;
var
  slOutput: TStringList;
begin
  slOutput := TStringList.Create;
  try
    Result := WinExecWithPipe(CmdLine, Dir, slOutput, dwExitCode);
    Output := slOutput.Text;
  finally
    slOutput.Free;
  end;
end;

// 创建 GUID 字符串
function CreateGuidString: string;
var
  P: PWideChar;
  GUID: TGUID;
begin
  CoCreateGuid(GUID);
  if not Succeeded(StringFromCLSID(GUID, P)) then
    Result := ''
  else
    Result := P;
  CoTaskMemFree(P);
end;

// 应用程序路径
function AppPath: string;
begin
  Result := _CnExtractFilePath(Application.ExeName);
end;

// 当前执行模块所在的路径
function ModulePath: string;
var
  ModName: array[0..MAX_PATH] of Char;
begin
  SetString(Result, ModName, GetModuleFileName(HInstance, ModName, SizeOf(ModName)));
  Result := _CnExtractFilePath(Result);
end;

const
  HKLM_CURRENT_VERSION_WINDOWS = 'Software\Microsoft\Windows\CurrentVersion';
  HKLM_CURRENT_VERSION_NT      = 'Software\Microsoft\Windows NT\CurrentVersion';

function RelativeKey(const Key: string): PChar;
begin
  Result := PChar(Key);
  if (Key <> '') and (Key[1] = '\') then
    Inc(Result);
end;

function RegReadStringDef(const RootKey: HKEY; const Key, Name, Def: string): string;
var
  RegKey: HKEY;
  Size: DWORD;
  StrVal: string;
  RegKind: DWORD;
begin
  Result := Def;
  if RegOpenKeyEx(RootKey, RelativeKey(Key), 0, KEY_READ, RegKey) = ERROR_SUCCESS then
  begin
    RegKind := 0;
    Size := 0;
    if RegQueryValueEx(RegKey, PChar(Name), nil, @RegKind, nil, @Size) = ERROR_SUCCESS then
      if RegKind in [REG_SZ, REG_EXPAND_SZ] then
      begin
        SetLength(StrVal, Size);
        if RegQueryValueEx(RegKey, PChar(Name), nil, @RegKind, PByte(StrVal), @Size) = ERROR_SUCCESS then
        begin
          SetLength(StrVal, StrLen(PChar(StrVal)));
          Result := StrVal;
        end;
      end;
    RegCloseKey(RegKey);
  end;
end;

function GetKeysInRegistryKey(const Key: string; List: TStrings): Boolean;
var
  Reg: TRegistry;
begin
  Result := False;
  Reg := TRegistry.Create(KEY_READ);
  try
    if Reg.OpenKey(Key, False) then
    begin
      Reg.GetKeyNames(List);
      Result := True;
    end;
  finally
    Reg.Free;
  end;
end;

{$ENDIF}

procedure StrResetLength(var S: string);
begin
  SetLength(S, StrLen(PChar(S)));
end;

{$IFDEF MSWINDOWS}

// 取 Program Files 目录
function GetProgramFilesDir: string;
begin
  Result := RegReadStringDef(HKEY_LOCAL_MACHINE, HKLM_CURRENT_VERSION_WINDOWS, 'ProgramFilesDir', '');
end;

// 取 Windows 目录
function GetWindowsDir: string;
var
  Required: Cardinal;
begin
  Result := '';
  Required := GetWindowsDirectory(nil, 0);
  if Required <> 0 then
  begin
    SetLength(Result, Required);
    GetWindowsDirectory(PChar(Result), Required);
    StrResetLength(Result);
  end;
end;

// 取临时文件路径
function GetWindowsTempPath: string;
var
  Required: Cardinal;
begin
  Result := '';
  Required := GetTempPath(0, nil);
  if Required <> 0 then
  begin
    SetLength(Result, Required);
    GetTempPath(Required, PChar(Result));
    StrResetLength(Result);
  end;
end;

// 返回一个临时文件名
function CnGetTempFileName(const Ext: string): string;
var
  Path: string;
begin
  Path := MakePath(GetWindowsTempPath);
  repeat
    Result := Path + IntToStr(Random(MaxInt)) + Ext;
  until not FileExists(Result);
end;

// 取系统目录
function GetSystemDir: string;
var
  Required: Cardinal;
begin
  Result := '';
  Required := GetSystemDirectory(nil, 0);
  if Required <> 0 then
  begin
    SetLength(Result, Required);
    GetSystemDirectory(PChar(Result), Required);
    StrResetLength(Result);
  end;
end;

function ShellGetFolder(const Name: string): string;
const
  RegPath = '\Software\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders';
var
  Reg: TRegistry;
  Folder: string;
begin
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if Reg.OpenKey(RegPath, False) then
      Folder := Reg.ReadString(Name);
  finally
    Reg.Free;
  end;
  Result := Folder;
end;

// 取我的文档目录
function GetMyDocumentsDir: string;
begin
  Result := ShellGetFolder('Personal');
end;

var
  _Kernel32Handle: HMODULE = HMODULE(0);
  _GetLongPathName: function (lpszShortPath: PChar; lpszLongPath: PChar;
    cchBuffer: DWORD): DWORD; stdcall;

function Kernel32Handle: HMODULE;
begin
  if _Kernel32Handle = HMODULE(0) then
    _Kernel32Handle := LoadLibrary(kernel32);
  Result := _Kernel32Handle;
end;

function ShellGetLongPathName(const Path: string): string;
var
  PIDL: PItemIDList;
  Desktop: IShellFolder;
  AnsiName: string;
  WideName: array [0..MAX_PATH] of WideChar;
  Eaten, Attr: ULONG;
begin
  Result := Path;
  if Path <> '' then
  begin
    if Succeeded(SHGetDesktopFolder(Desktop)) then
    begin
      MultiByteToWideChar(CP_ACP, MB_PRECOMPOSED, _CnPChar(Path), -1, WideName, MAX_PATH);
      if Succeeded(Desktop.ParseDisplayName(0, nil, WideName, Eaten, PIDL, Attr)) then
      try
        SetLength(AnsiName, MAX_PATH);
        if SHGetPathFromIDList(PIDL, PChar(AnsiName)) then
          StrResetLength(AnsiName);
        Result := AnsiName;
      finally
        CoTaskMemFree(PIDL);
      end;
    end;
  end;
end;

// 短文件名转长文件名
function ShortNameToLongName(const FileName: string): string;
const
{$IFDEF UNICODE}
  SCnGetLongPathName = 'GetLongPathNameW';
{$ELSE}
  SCnGetLongPathName = 'GetLongPathNameA';
{$ENDIF}
begin
  Result := FileName;
  if not Assigned(_GetLongPathName) then
    _GetLongPathName := GetProcAddress(Kernel32Handle, SCnGetLongPathName);
  if Assigned(_GetLongPathName) then
  begin
    SetLength(Result, MAX_PATH);
    SetLength(Result, _GetLongPathName(PChar(FileName), PChar(Result), MAX_PATH));
  end
  else
  begin
    Result := ShellGetLongPathName(FileName);
  end;
end;

// 长文件名转短文件名
function LongNameToShortName(const FileName: string): string;
var
  Buf: PChar;
  BufSize: Integer;
begin
  BufSize := GetShortPathName(PChar(FileName), nil, 0) + 1;
  GetMem(Buf, BufSize * SizeOf(Char));
  try
    GetShortPathName(PChar(FileName), Buf, BufSize);
    Result := Buf;
  finally
    FreeMem(Buf);
  end;
end;

// 取得真实长文件名，包含大小写
function GetTrueFileName(const FileName: string): string;
var
  AName: string;
  FindName: string;

  function DoFindFile(const FName: string): string;
  var
    F: TSearchRec;
  begin
    if SysUtils.FindFirst(FName, faAnyFile, F) = 0 then
      Result := F.Name
    else
      Result := _CnExtractFileName(FName);
    SysUtils.FindClose(F);
  end;
begin
  AName := MakeDir(FileName);
  if (Length(AName) > 3) and (AName[2] = ':') then
  begin
    Result := '';
    while Length(AName) > 3 do
    begin
      FindName := DoFindFile(AName);

      if FindName = '' then
      begin
        Result := AName;
        Exit;
      end;

      if Result = '' then
        Result := FindName
      else
        Result := FindName + '\' + Result;

      AName := _CnExtractFileDir(AName);
    end;

    Result := UpperCase(AName) + Result;
  end
  else
    Result := AName;
end;

// 查找可执行文件的完整路径
function FindExecFile(const AName: string; var AFullName: string): Boolean;
var
  fn: array[0..MAX_PATH] of Char;
  pc: PChar;
begin
  if (0 = SearchPath(nil, PChar(AName), '.exe', Length(fn), fn, pc)) and
     (0 = SearchPath(nil, PChar(AName), '.com', Length(fn), fn, pc)) and
     (0 = SearchPath(nil, PChar(AName), '.bat', Length(fn), fn, pc)) then
  begin
    Result := False;
  end
  else
  begin
    Result := True;
    AFullName := fn;
  end;
end;

function PidlFree(var IdList: PItemIdList): Boolean;
var
  Malloc: IMalloc;
begin
  Result := False;
  if IdList = nil then
    Result := True
  else
  begin
    if Succeeded(SHGetMalloc(Malloc)) and (Malloc.DidAlloc(IdList) > 0) then
    begin
      Malloc.Free(IdList);
      IdList := nil;
      Result := True;
    end;
  end;
end;

function PidlToPath(IdList: PItemIdList): string;
begin
  SetLength(Result, MAX_PATH);
  if SHGetPathFromIdList(IdList, PChar(Result)) then
    StrResetLength(Result)
  else
    Result := '';
end;

// 取得系统特殊文件夹位置，Folder 使用在 ShlObj 中定义的标识，如 CSIDL_DESKTOP
function GetSpecialFolderLocation(const Folder: Integer): string;
var
  FolderPidl: PItemIdList;
begin
  if Succeeded(SHGetSpecialFolderLocation(0, Folder, FolderPidl)) then
  begin
    Result := PidlToPath(FolderPidl);
    PidlFree(FolderPidl);
  end
  else
    Result := '';
end;

{$ENDIF}

// 目录尾加'\'修正
function AddDirSuffix(const Dir: string): string;
begin
  Result := Trim(Dir);
  if Result = '' then Exit;
  if not IsPathDelimiter(Result, Length(Result)) then
    Result := Result + {$IFDEF MSWINDOWS} '\'; {$ELSE} '/'; {$ENDIF};
end;

// 目录尾加'\'修正
function MakePath(const Dir: string): string;
begin
  Result := AddDirSuffix(Dir);
end;

// 路径尾去掉 '\'
function MakeDir(const Path: string): string;
begin
  Result := Trim(Path);
  if Result = '' then Exit;
  if CharInSet(Result[Length(Result)], ['/', '\']) then
    Delete(Result, Length(Result), 1);
end;

// 路径中的 '\' 转成 '/'
function GetUnixPath(const Path: string): string;
begin
  Result := StringReplace(Path, '\', '/', [rfReplaceAll]);
end;

// 路径中的 '/' 转成 '\'
function GetWinPath(const Path: string): string;
begin
  Result := StringReplace(Path, '/', '\', [rfReplaceAll]);
end;

function PointerXX(var X: PAnsiChar): PAnsiChar;
begin
  Result := X;
  Inc(X);
end;
//{$ELSE}
//asm
//  {
//  EAX = X
//  }
//  MOV EDX, [EAX]
//  INC dword ptr [EAX]
//  MOV EAX, EDX
//end;
//{$ENDIF}

function Evaluate(var X: AnsiChar; const Value: AnsiChar): AnsiChar;
begin
  X := Value;
  Result := X;
end;
//{$ELSE}
//asm
//  {
//  EAX = X
//  EDX = Value (DL)
//  }
//  MOV [EAX], DL
//  MOV AL, [EAX]
//end;
//{$ENDIF}

// 文件名是否与通配符匹配，返回值为 0 表示匹配
function FileNameMatch(Pattern, FileName: PAnsiChar): Integer;
var
  p, n: PAnsiChar;
  c: AnsiChar;
begin
  p := Pattern;
  n := FileName;

  while Evaluate(c, PointerXX(p)^) <> #0 do
  begin
    case c of
      '?': begin
          if n^ = '.' then
          begin
            while (p^ <> '.') and (p^ <> #0) do
            begin
              if (p^ <> '?') and (p^ <> '*') then
              begin
                Result := -1;
                Exit;
              end;
              Inc(p);
            end;
          end
          else
          begin
            if n^ <> #0 then
              Inc(n);
          end;
        end;

      '>': begin
          if n^ = '.' then
          begin
            if ((n + 1)^ = #0) and (FileNameMatch(p, n+1) = 0) then
            begin
              Result := 0;
              Exit;
            end;
            if FileNameMatch(p, n) = 0 then
            begin
              Result := 0;
              Exit;
            end;
            Result := -1;
            Exit;
          end;
          if n^ = #0 then
          begin
            Result := FileNameMatch(p, n);
            Exit;
          end;
          Inc(n);
        end;

      '*': begin
          while n^ <> #0 do
          begin
            if FileNameMatch(p, n) = 0 then
            begin
              Result := 0;
              Exit;
            end;
            Inc(n);
          end;
        end;

      '<': begin
          while n^ <> #0 do
          begin
            if FileNameMatch(p, n) = 0 then
            begin
              Result := 0;
              Exit;
            end;
            if (n^ = '.') and (StrScan(n + 1, '.') = nil) then
            begin
              Inc(n);
              Break;
            end;
            Inc(n);
          end;
        end;

      '"': begin
          if (n^ = #0) and (FileNameMatch(p, n) = 0) then
          begin
            Result := 0;
            Exit;
          end;
          if n^ <> '.' then
          begin
            Result := -1;
            Exit;
          end;
          Inc(n);
        end;
    else
      if (c = '.') and (n^ = #0) then
      begin
        while p^ <> #0 do
        begin
          if (p^ = '*') and ((p + 1)^ = #0) then
          begin
            Result := 0;
            Exit;
          end;
          if p^ <> '?' then
          begin
            Result := -1;
            Exit;
          end;
          Inc(p);
        end;
        Result := 0;
        Exit;
      end;
      if c <> n^ then
      begin
        Result := -1;
        Exit;
      end;
      Inc(n);
    end;
  end;

  if n^ = #0 then
  begin
    Result := 0;
    Exit;
  end;

  Result := -1;
end;

// 文件名是否与扩展名通配符匹配
function MatchExt(const S, Ext: string): Boolean;
begin
  if S = '.*' then
  begin
    Result := True;
    Exit;
  end;

  Result := FileNameMatch(_CnPChar(S), _CnPChar(Ext)) = 0;
end;

// 文件名是否与通配符匹配
function MatchFileName(const S, FN: string): Boolean;
begin
  if S = '*.*' then
  begin
    Result := True;
    Exit;
  end;

  Result := FileNameMatch(_CnPChar(S), _CnPChar(FN)) = 0;
end;

// 得到大小写是否敏感的字符串
function _CaseSensitive(const CaseSensitive: Boolean; const S: string): string;
begin
  if CaseSensitive then
    Result := S
  else
    Result := AnsiUpperCase(S);
end;

// 转换扩展名通配符字符串为通配符列表
procedure FileExtsToStrings(const FileExts: string; ExtList: TStrings; CaseSensitive: Boolean);
var
  Exts: string;
  I: Integer;
begin
  Exts := StringReplace(FileExts, ';', ',', [rfReplaceAll]);
  ExtList.CommaText := Exts;

  for I := 0 to ExtList.Count - 1 do
  begin
    if StrScan(PChar(ExtList[I]), '.') <> nil then
    begin
      ExtList[I] := _CaseSensitive(CaseSensitive, _CnExtractFileExt(ExtList[I]));
    end
    else
    begin
      ExtList[I] := '.' + _CaseSensitive(CaseSensitive, ExtList[I]);
    end;
    if ExtList[I] = '.*' then
    begin
      if I > 0 then
        ExtList.Exchange(0, I);
      Exit;
    end;
  end;
end;

// 文件名是否匹配扩展名通配符
function FileMatchesExts(const FileName, FileExts: string; CaseSensitive: Boolean): Boolean;
var
  ExtList: TStrings;
  FExt: string;
  I: Integer;
begin
  ExtList := TStringList.Create;
  try
    FileExtsToStrings(FileExts, ExtList, CaseSensitive);

    FExt := _CaseSensitive(CaseSensitive, _CnExtractFileExt(FileName));
    Result := False;
    for I := 0 to ExtList.Count - 1 do
    begin
      if MatchExt(ExtList[I], FExt) then
      begin
        Result := True;
        Exit;
      end;
    end;
  finally
    ExtList.Free;
  end;
end;

// 文件名是否匹配扩展名通配符
function FileMatchesExts(const FileName: string; ExtList: TStrings): Boolean;
var
  FExt: string;
  I: Integer;
begin
  FExt := _CaseSensitive(False, _CnExtractFileExt(FileName));

  Result := False;
  for I := 0 to ExtList.Count - 1 do
  begin
    if MatchExt(ExtList[I], FExt) then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

// 转换文件通配符字符串为通配符列表
procedure FileMasksToStrings(const FileMasks: string; MaskList: TStrings; CaseSensitive: Boolean);
var
  Exts: string;
  I: Integer;
begin
  Exts := StringReplace(FileMasks, ';', ',', [rfReplaceAll]);
  MaskList.CommaText := Exts;

  for I := 0 to MaskList.Count - 1 do
  begin
    if StrScan(PChar(MaskList[I]), '.') <> nil then
    begin
      if MaskList[I][1] = '.' then
        MaskList[I] := '*' + _CaseSensitive(CaseSensitive, MaskList[I])
      else
        MaskList[I] := _CaseSensitive(CaseSensitive, MaskList[I]);
    end
    else
    begin
      MaskList[I] := '*.' + _CaseSensitive(CaseSensitive, MaskList[I]);
    end;
    if MaskList[I] = '*.*' then
    begin
      if I > 0 then
        MaskList.Exchange(0, I);
      Exit;
    end;
  end;
end;

// 文件名是否匹配通配符
function FileMatchesMasks(const FileName, FileMasks: string; CaseSensitive: Boolean): Boolean;
var
  MaskList: TStrings;
  FFileName: string;
  I: Integer;
begin
  MaskList := TStringList.Create;
  try
    FileMasksToStrings(FileMasks, MaskList, CaseSensitive);

    FFileName := _CaseSensitive(CaseSensitive, _CnExtractFileName(FileName));
    Result := False;
    for I := 0 to MaskList.Count - 1 do
    begin
      if MatchFileName(MaskList[I], FFileName) then
      begin
        Result := True;
        Exit;
      end;
    end;
  finally
    MaskList.Free;
  end;
end;

// 文件名是否匹配通配符
function FileMatchesMasks(const FileName: string; MaskList: TStrings): Boolean;
var
  FFileName: string;
  I: Integer;
begin
  FFileName := _CaseSensitive(False, _CnExtractFileName(FileName));

  Result := False;
  for I := 0 to MaskList.Count - 1 do
  begin
    if MatchFileName(_CaseSensitive(False, MaskList[I]), FFileName) then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

{$IFDEF MSWINDOWS}

// 判断文件是否正在使用
function IsFileInUse(const FName: string): Boolean;
var
  HFileRes: HFILE;
begin
  Result := False;
  if not FileExists(FName) then
    Exit;
  HFileRes := CreateFile(PChar(FName), GENERIC_READ or GENERIC_WRITE, 0,
    nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
  Result := (HFileRes = INVALID_HANDLE_VALUE);
  if not Result then
    CloseHandle(HFileRes);
end;

{$ENDIF}

// 判断文件是否为 Ascii 文件
function IsAscii(const FileName: string): Boolean;
const
  SETT = 2048;
var
  I: Integer;
  AFile: File;
  Bool: Boolean;
  TotSize, IncSize, ReadSize: Integer;
  C: array[0..SETT] of Byte;
begin
  Result := False;
  if FileExists(FileName) then
  begin
    {$I-}
    AssignFile(AFile, FileName);
    Reset(AFile, 1);
    TotSize := FileSize(AFile);
    IncSize := 0;
    Bool := True;
    while (IncSize < TotSize) and Bool do
    begin
      ReadSize := SETT;
      if IncSize + ReadSize > TotSize then
        ReadSize := TotSize - IncSize;
      IncSize := IncSize + ReadSize;
      BlockRead(AFile, C, ReadSize);
      for I := 0 to ReadSize-1 do // Iterate
        if (C[I] < 32) and (not(C[I] in [9, 10, 13, 26])) then Bool := False;
    end; // while
    CloseFile(AFile);
    {$I+}
    if IOResult <> 0 then
      Result := False
    else
      Result := Bool;
  end;
end;

// 判断文件是否是有效的文件名
function IsValidFileName(const Name: string): Boolean;
var
  I: Integer;
begin
  Result := False;

  if (Name = '') or (Length(Name) > MAX_PATH) then
    Exit;

  for I := 1 to Length(Name) do
  begin
    if CharInSet(Name[I], InvalidFileNameChar) then
      Exit;
  end;
  Result := True;
end;

// 返回有效的文件名
function GetValidFileName(const Name: string): string;
var
  I: Integer;
begin
  Result := Name;
  for I := Length(Result) downto 1 do
  begin
    if CharInSet(Result[I], InvalidFileNameChar) then
      Delete(Result, I, 1);
  end;
  if Length(Result) > MAX_PATH - 1 then
    Result := Copy(Result, 1, MAX_PATH - 1);
end;

{$IFDEF MSWINDOWS}

// 设置文件时间
function SetFileDate(const FileName: string; CreationTime, LastWriteTime, LastAccessTime:
  TFileTime): Boolean;
var
  FileHandle: Integer;
begin
  FileHandle := FileOpen(FileName, fmOpenWrite or fmShareDenyNone);
  if FileHandle > 0 then
  begin
    SetFileTime(FileHandle, @CreationTime, @LastAccessTime, @LastWriteTime);
    FileClose(FileHandle);
    Result := True;
  end
  else
    Result := False;
end;

// 取文件时间
function GetFileDate(const FileName: string; var CreationTime, LastWriteTime, LastAccessTime:
  TFileTime): Boolean;
var
  FileHandle: Integer;
begin
  FileHandle := FileOpen(FileName, fmOpenRead or fmShareDenyNone);
  if FileHandle > 0 then
  begin
    GetFileTime(FileHandle, @CreationTime, @LastAccessTime, @LastWriteTime);
    FileClose(FileHandle);
    Result := True;
  end
  else
    Result := False;
end;

// 取得与文件相关的图标
// FileName: e.g. "e:\hao\a.txt"
// 成功则返回True
function GetFileIcon(const FileName: string; var Icon: TIcon): Boolean;
var
  SHFileInfo: TSHFileInfo;
  h: HWND;
begin
  if not Assigned(Icon) then
    Icon := TIcon.Create;
  h := SHGetFileInfo(PChar(FileName),
    0,
    SHFileInfo,
    SizeOf(SHFileInfo),
    SHGFI_ICON or SHGFI_SYSICONINDEX);
  Icon.Handle := SHFileInfo.hIcon;
  Result := (h <> 0);
end;

// 文件时间转本地日期时间
function FileTimeToDateTime(const FileTime: TFileTime): TDateTime;
var
  SystemTime: TSystemTime;
begin
  SystemTime := FileTimeToLocalSystemTime(FileTime);
  with SystemTime do
    Result := EncodeDate(wYear, wMonth, wDay) + EncodeTime(wHour, wMinute,
      wSecond, wMilliseconds);
end;

// 本地日期时间转文件时间
function DateTimeToFileTime(const DateTime: TDateTime): TFileTime;
var
  SystemTime: TSystemTime;
begin
  with SystemTime do
  begin
    DecodeDate(DateTime, wYear, wMonth, wDay);
    DecodeTime(DateTime, wHour, wMinute, wSecond, wMilliseconds);
  end;
  Result := LocalSystemTimeToFileTime(SystemTime);
end;

// 文件时间转本地时间
function FileTimeToLocalSystemTime(FTime: TFileTime): TSystemTime;
var
  STime: TSystemTime;
begin
  FileTimeToLocalFileTime(FTime, FTime);
  FileTimeToSystemTime(FTime, STime);
  Result := STime;
end;

// 本地时间转文件时间
function LocalSystemTimeToFileTime(STime: TSystemTime): TFileTime;
var
  FTime: TFileTime;
begin
  SystemTimeToFileTime(STime, FTime);
  LocalFileTimeToFileTime(FTime, FTime);
  Result := FTime;
end;

const
  MinutesPerDay     = 60 * 24;
  SecondsPerDay     = MinutesPerDay * 60;

// UTC 时间转本地时间
function DateTimeToLocalDateTime(DateTime: TDateTime): TDateTime;
var
  TimeZoneInfo: TTimeZoneInformation;
begin
  FillChar(TimeZoneInfo, SizeOf(TimeZoneInfo), #0);
  if GetTimeZoneInformation(TimeZoneInfo) = TIME_ZONE_ID_DAYLIGHT then
    Result := DateTime - ((TimeZoneInfo.Bias + TimeZoneInfo.DaylightBias) / MinutesPerDay)
  else
    Result := DateTime - (TimeZoneInfo.Bias / MinutesPerDay);
end;

// 本地时间转 UTC 时间
function LocalDateTimeToDateTime(DateTime: TDateTime): TDateTime;
var
  TimeZoneInfo: TTimeZoneInformation;
begin
  FillChar(TimeZoneInfo, SizeOf(TimeZoneInfo), #0);
  if GetTimeZoneInformation(TimeZoneInfo) = TIME_ZONE_ID_DAYLIGHT then
    Result := DateTime + ((TimeZoneInfo.Bias + TimeZoneInfo.DaylightBias) / MinutesPerDay)
  else
    Result := DateTime + (TimeZoneInfo.Bias / MinutesPerDay);
end;

// 把程序钉到 Windows7 任务栏，参数为程序路径与文件名
procedure PinAppToWin7Taskbar(const Path, App: string);
var
  Shell, Folder, FolderItem, ItemVerbs: Variant;
  vPath, vApp: Variant;
  I: Integer;
  Str: String;
  H: HINST;
  PinName: array[0..255] of Char;
begin
  Shell := CreateOleObject('Shell.Application');
  vPath := Path;
  Folder := Shell.NameSpace(vPath);
  vApp := App;
  FolderItem := Folder.ParseName(vApp);
  ItemVerbs := FolderItem.Verbs;

  H := LoadLibrary('Shell32.dll');
  LoadString(H, 5386, PinName, 256);
  FreeLibrary(H);

  for I := 1 to ItemVerbs.Count do
  begin
    Str := ItemVerbs.Item(I).Name;
    if SameText(Str, PinName) then
      ItemVerbs.Item(I).DoIt;
  end;
end;

{$ENDIF}

{$IFDEF COMPILER5}
const
  LessThanValue = Low(TValueRelationship);
  EqualsValue = 0;
  GreaterThanValue = High(TValueRelationship);

function CompareValue(const A, B: Int64): TValueRelationship;
begin
  if A = B then
    Result := EqualsValue
  else if A < B then
    Result := LessThanValue
  else
    Result := GreaterThanValue;
end;

// AText 是否以 ASubText 开头
function AnsiStartsText(const ASubText, AText: string): Boolean;
begin
  Result := AnsiPos(AnsiUpperCase(ASubText), AnsiUpperCase(AText)) = 1;
end;

function AnsiReplaceText(const AText, AFromText, AToText: string): string;
begin
  Result := StringReplace(AText, AFromText, AToText, [rfReplaceAll, rfIgnoreCase]);
end;
{$ENDIF}

// StringReplace 无法处理字符串中的 #0，新写一个全替换的版本，不处理大小写}
function ReplaceAllInString(const S: string; OldPattern, NewPattern: string): string;
var
  SchStr, PatStr, NewStr: string;
  Offset: Integer;
begin
  Result := '';
  SchStr := S;
  PatStr := OldPattern;
  NewStr := S;

  while SchStr <> '' do
  begin
    Offset := Pos(PatStr, SchStr);
    if Offset = 0 then
    begin
      Result := Result + NewStr;
      Break;
    end;
    Result := Result + Copy(NewStr, 1, Offset - 1) + NewPattern;
    NewStr := Copy(NewStr, Offset + Length(OldPattern), MaxInt);
    SchStr := Copy(SchStr, Offset + Length(PatStr), MaxInt);
  end;
end;

{$IFNDEF COMPILER7_UP}
// AText 是否包含 ASubText
function AnsiContainsText(const AText, ASubText: string): Boolean;
begin
  Result := AnsiPos(AnsiUpperCase(ASubText), AnsiUpperCase(AText)) > 0;
end;
{$ENDIF}

// 比较 SubText 在两个字符串中出现的位置的大小，如果相等则比较字符串本身，忽略大小写
function AnsiCompareTextPos(const ASubText, AText1, AText2: string): TValueRelationship;
begin
  Result := 0;
  if ASubText <> '' then
    Result := CompareValue(AnsiPos(AnsiUpperCase(ASubText), AnsiUpperCase(AText1)),
      AnsiPos(AnsiUpperCase(ASubText), AnsiUpperCase(AText2)));
  if Result = 0 then
    Result := AnsiCompareText(AText1, AText2);
end;

function CompareTextPos(const ASubText, AText1, AText2: string): TValueRelationship;
begin
  Result := 0;
  if ASubText <> '' then
    Result := CompareValue(Pos(UpperCase(ASubText), UpperCase(AText1)),
      Pos(UpperCase(ASubText), UpperCase(AText2)));
  if Result = 0 then
    Result := CompareText(AText1, AText2);
end;

// 比较 SubText 在两个字符串中出现的位置的大小，位置不反序，相等则比较字符串本身，本身可反序，忽略大小写
function CompareTextWithPos(const ASubText, AText1, AText2: string;
  Reverse: Boolean): TValueRelationship;
var
  P1, P2: Integer;
begin
  Result := 0;
  if ASubText <> '' then
  begin
    P1 := Pos(UpperCase(ASubText), UpperCase(AText1));
    P2 := Pos(UpperCase(ASubText), UpperCase(AText2));

    if P1 = P2 then // 子串位置相同或都没有，比较字符串本身，可反序
    begin
      Result := CompareStr(AText1, AText2);
      if Reverse then
        Result := -Result;
    end
    else if (P1 = 0) or (P2 = 0) then // 一个有一个没有，无需反序，有的必然靠前
    begin
      if P1 = 0 then
        Result := 1
      else if P2 = 0 then
        Result := -1;
    end
    else // 都有但不同，比较子串位置，无需反序
    begin
      Result := P1 - P2;
    end;
  end
  else // 子串为空，纯比较字符串，可反序
  begin
    Result := CompareStr(AText1, AText2);
    if Reverse then
      Result := -Result;
  end;
end;

// 非 Ansi 方式的字符串替换
function StringReplaceNonAnsi(const S, OldPattern, NewPattern: string;
  Flags: TReplaceFlags): string;
var
  SearchStr, Patt, NewStr: string;
  Offset: Integer;
begin
  if rfIgnoreCase in Flags then
  begin
    SearchStr := UpperCase(S);
    Patt := UpperCase(OldPattern);
  end else
  begin
    SearchStr := S;
    Patt := OldPattern;
  end;
  NewStr := S;
  Result := '';
  while SearchStr <> '' do
  begin
    Offset := Pos(Patt, SearchStr);
    if Offset = 0 then
    begin
      Result := Result + NewStr;
      Break;
    end;
    Result := Result + Copy(NewStr, 1, Offset - 1) + NewPattern;
    NewStr := Copy(NewStr, Offset + Length(OldPattern), MaxInt);
    if not (rfReplaceAll in Flags) then
    begin
      Result := Result + NewStr;
      Break;
    end;
    SearchStr := Copy(SearchStr, Offset + Length(Patt), MaxInt);
  end;
end;

// KMP 字符串匹配算法，返回 S 中第一次出现 Pattern 的位置，位置以 1 开始，未匹配则返回 0
function StringKMP(const Pattern, S: string): Integer;
var
  LP, LS, I, J: Integer;
  N: array of Integer;
  PS, PP: PChar;

  procedure CalcNext(P: PChar);
  var
    II, JJ: Integer;
  begin
    N[0] := -1;
    II := 0;
    JJ := -1;

    while II < LP do
    begin
      if (JJ = -1) or (P[II] = P[JJ]) then
      begin
        Inc(II);
        Inc(JJ);
        N[II] := JJ;
      end
      else
        JJ := N[JJ];
    end;
  end;

begin
  Result := 0;
  if (Pattern = '') or (S = '') then
    Exit;

  LP := Length(Pattern);
  LS := Length(S);
  if LS < LP then
    Exit;

  PS := PChar(S);
  PP := PChar(Pattern);

  SetLength(N, LP + 1); // 注意 N 的下标从 0 到 LP - 1，但 CalcNext 中会用到 N[LP]
  CalcNext(PP);

  I := 0;
  J := 0;
  while (I < LS) and (J < LP) do
  begin
    if (J = -1) or (PS[I] = PP[J]) then
    begin
      Inc(I);
      Inc(J);
    end
    else
      J := N[J];
  end;

  SetLength(N, 0);
  if J = LP then
    Result := I - J + 1;
end;

// 创建备份文件
function CreateBakFile(const FileName, Ext: string): Boolean;
var
  BakFileName: string;
  AExt: string;
begin
  if (Ext <> '') and (Ext[1] = '.') then
    AExt := Ext
  else
    AExt := '.' + Ext;
  BakFileName := FileName + AExt;
{$IFDEF MSWINDOWS}
  Result := CopyFile(PChar(FileName), PChar(BakFileName), False);
{$ELSE}
  Result := True;
  try
    TFile.Copy(FileName, BakFileName, False);
  except
    Result := False;
  end;
{$ENDIF}
end;

// 删除整个目录
function Deltree(const Dir: string; DelRoot: Boolean; DelEmptyDirOnly: Boolean): Boolean;
var
  SR: TSearchRec;
  FR: Integer;
begin
  Result := True;
  if not DirectoryExists(Dir) then
    Exit;

  FR := FindFirst(AddDirSuffix(Dir) + '*.*', faAnyFile, SR);
  try
    while FR = 0 do
    begin
      if (SR.Name <> '.') and (SR.Name <> '..') then
      begin
{$IFDEF MSWINDOWS}
        SetFileAttributes(PChar(AddDirSuffix(Dir) + SR.Name), FILE_ATTRIBUTE_NORMAL);
{$ELSE}
        TFile.SetAttributes(AddDirSuffix(Dir) + SR.Name, [TFileAttribute.faNormal]);
{$ENDIF}

        if SR.Attr and faDirectory = faDirectory then
          Result := Deltree(AddDirSuffix(Dir) + SR.Name, True, DelEmptyDirOnly)
        else if not DelEmptyDirOnly then
          Result := SysUtils.DeleteFile(AddDirSuffix(Dir) + SR.Name);
      end;
      FR := FindNext(SR);
    end;
  finally
    SysUtils.FindClose(SR);
  end;

  if DelRoot then
    Result := RemoveDir(Dir);
end;

// 删除整个目录中的空目录，DelRoot 表示是否删除目录本身
procedure DelEmptyTree(const Dir: string; DelRoot: Boolean = True);
var
  sr: TSearchRec;
  fr: Integer;
begin
  fr := FindFirst(AddDirSuffix(Dir) + '*.*', faDirectory, sr);
  try
    while fr = 0 do
    begin
      if (sr.Name <> '.') and (sr.Name <> '..') and (sr.Attr and faDirectory
        = faDirectory) then
      begin
{$IFDEF MSWINDOWS}
        SetFileAttributes(PChar(AddDirSuffix(Dir) + sr.Name), FILE_ATTRIBUTE_NORMAL);
{$ELSE}
        TFile.SetAttributes(AddDirSuffix(Dir) + SR.Name, [TFileAttribute.faNormal]);
{$ENDIF}

        DelEmptyTree(AddDirSuffix(Dir) + sr.Name, True);
      end;
      fr := FindNext(sr);
    end;
  finally
    SysUtils.FindClose(sr);
  end;

  if DelRoot then
    RemoveDir(Dir);
end;

// 取文件夹下的直系文件列表，文件名不包含路径名。不搜索子目录，返回文件数
function GetDirFiles(const Dir: string; FileNames: TStrings): Integer;
var
  Sr: TSearchRec;
  Fr: Integer;
begin
  Result := 0;
  if FileNames <> nil then
    FileNames.Clear;

  Fr := FindFirst(AddDirSuffix(Dir) + '*.*', faAnyFile, Sr);
  while Fr = 0 do
  begin
    if (Sr.Name <> '.') and (Sr.Name <> '..')// 不是目录
      {$IFDEF MSWINDOWS} and (FILE_ATTRIBUTE_DIRECTORY and Sr.Attr = 0) {$ELSE}
      and (S_IFDIR and Sr.Attr = 0) {$ENDIF}
    then
    begin
      Inc(Result);
      if FileNames <> nil then
        FileNames.Add(Sr.Name);
    end;
    Fr := FindNext(Sr);
  end;
  SysUtils.FindClose(Sr);
end;

{$IFDEF MSWINDOWS}

// 根据指定类名查找窗体
function FindFormByClass(AClass: TClass): TForm;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Screen.FormCount - 1 do
  begin
    if Screen.Forms[I] is AClass then
    begin
      Result := Screen.Forms[I];
      Exit;
    end;
  end;
end;



// 当前是否有模态窗口存在
function ModalFormExists: Boolean;
var
  I: Integer;
begin
  for I := 0 to Screen.CustomFormCount - 1 do
  begin
    if fsModal in Screen.CustomForms[I].FormState then
    begin
      Result := True;
      Exit;
    end
  end;
  Result := False;
end;

var
  FindAbort: Boolean;

// 查找指定目录下文件
function FindFile(const Path: string; const FileName: string = '*.*';
  Proc: TFindCallBack = nil; DirProc: TDirCallBack = nil; bSub: Boolean = True;
  bMsg: Boolean = True): Boolean;

  procedure DoFindFile(const Path, SubPath: string; const FileName: string;
    Proc: TFindCallBack; DirProc: TDirCallBack; bSub: Boolean;
    bMsg: Boolean);
  var
    APath: string;
    Info: TSearchRec;
    Succ: Integer;
  begin
    FindAbort := False;
    APath := MakePath(MakePath(Path) + SubPath);
    Succ := FindFirst(APath + FileName, faAnyFile - faVolumeID, Info);
    try
      while Succ = 0 do
      begin
        if (Info.Name <> '.') and (Info.Name <> '..') then
        begin
          if (Info.Attr and faDirectory) <> faDirectory then
          begin
            if Assigned(Proc) then
              Proc(APath + Info.FindData.cFileName, Info, FindAbort);
          end
        end;
        if bMsg then
          Application.ProcessMessages;
        if FindAbort then
          Exit;
        Succ := FindNext(Info);
      end;
    finally
      SysUtils.FindClose(Info);
    end;

    if bSub then
    begin
      Succ := FindFirst(APath + '*.*', faAnyFile - faVolumeID, Info);
      try
        while Succ = 0 do
        begin
          if (Info.Name <> '.') and (Info.Name <> '..') and
            (Info.Attr and faDirectory = faDirectory) then
          begin
            if Assigned(DirProc) then
              DirProc(MakePath(SubPath) + Info.Name);
            DoFindFile(Path, MakePath(SubPath) + Info.Name, FileName, Proc,
              DirProc, bSub, bMsg);
            if FindAbort then
              Exit;
          end;
          Succ := FindNext(Info);
        end;
      finally
        SysUtils.FindClose(Info);
      end;
    end;
  end;

begin
  DoFindFile(Path, '', FileName, Proc, DirProc, bSub, bMsg);
  Result := not FindAbort;
end;

// 调用 SearchFile 在当前目录、系统目录和 PATH 变量中查找指定扩展名的文件。
// FileName 为不带扩展名的文件名，Ext 为扩展名，成功返回全路径文件名，失败返回空。
function CnSearchFile(const FileName: string; const Ext: string = '.exe'): string;
var
  FN: array[0..MAX_PATH] of Char;
  PC: PChar;
begin
  Result := '';
  if SearchPath(nil, PChar(FileName), PChar(Ext), Length(FN), FN, PC) <> 0 then
    Result := FN;
end;

// 文件打开方式
function OpenWith(const FileName: string): Integer;
begin
  Result := ShellExecute(Application.Handle, 'open', 'rundll32.exe',
    PChar('shell32.dll,OpenAs_RunDLL ' + FileName), '', SW_SHOW);
end;

// 检查指定的应用程序是否正在运行
// 作者：周劲羽 2002.08.12
function CheckAppRunning(const FileName: string; var Running: Boolean): Boolean;
var
  hSnap: THandle;
  ppe: TProcessEntry32;
  AName: string;
begin
  Result := False;
  AName := Trim(FileName);
  if AName = '' then Exit;              // 如果为空直接退出
  if _CnExtractFileExt(FileName) = '' then // 默认扩展名为 EXE
    AName := AName + '.EXE';
  hSnap := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0); // 创建当前进程快照
  if hSnap <> INVALID_HANDLE_VALUE then
  try
    ppe.dwSize := SizeOf(TProcessEntry32);
    if Process32First(hSnap, ppe) then  // 取第一个进程信息
      repeat
        if AnsiCompareText(_CnExtractFileName(ppe.szExeFile), AName) = 0 then
        begin                           // 比较应用程序名
          Running := True;
          Result := True;
          Exit;
        end;
      until not Process32Next(hSnap, ppe); // 取下一个进程信息
    Result := GetLastError = ERROR_NO_MORE_FILES; // 判断查找是否正常结束
  finally
    CloseHandle(hSnap);                 // 关闭句柄
  end;
end;

// 取文件版本号
function GetFileVersionNumber(const FileName: string): TVersionNumber;
var
  VersionInfoBufferSize: DWORD;
  dummyHandle: DWORD;
  VersionInfoBuffer: Pointer;
  FixedFileInfoPtr: PVSFixedFileInfo;
  VersionValueLength: UINT;
begin
  FillChar(Result, SizeOf(Result), 0);
  if not FileExists(FileName) then
    Exit;

  VersionInfoBufferSize := GetFileVersionInfoSize(PChar(FileName), dummyHandle);
  if VersionInfoBufferSize = 0 then
    Exit;

  GetMem(VersionInfoBuffer, VersionInfoBufferSize);
  try
    try
      Win32Check(GetFileVersionInfo(PChar(FileName), dummyHandle,
        VersionInfoBufferSize, VersionInfoBuffer));
      Win32Check(VerQueryValue(VersionInfoBuffer, '\',
        Pointer(FixedFileInfoPtr), VersionValueLength));
    except
      Exit;
    end;
    Result.Major := FixedFileInfoPtr^.dwFileVersionMS shr 16;
    Result.Minor := FixedFileInfoPtr^.dwFileVersionMS;
    Result.Release := FixedFileInfoPtr^.dwFileVersionLS shr 16;
    Result.Build := FixedFileInfoPtr^.dwFileVersionLS;
  finally
    FreeMem(VersionInfoBuffer);
  end;
end;

// 取文件版本字符串
function GetFileVersionStr(const FileName: string): string;
begin
  with GetFileVersionNumber(FileName) do
    Result := Format('%d.%d.%d.%d', [Major, Minor, Release, Build]);
end;

// 取文件信息
function GetFileInfo(const FileName: string; var FileSize: Int64;
  var FileTime: TDateTime): Boolean;
var
  Handle: THandle;
  FindData: TWin32FindData;
begin
  Result := False;
  Handle := FindFirstFile(PChar(FileName), FindData);
  if Handle <> INVALID_HANDLE_VALUE then
  begin
    Windows.FindClose(Handle);
    if (FindData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) = 0 then
    begin
      Int64Rec(FileSize).Lo := FindData.nFileSizeLow;
      Int64Rec(FileSize).Hi := FindData.nFileSizeHigh;
      FileTime := FileTimeToDateTime(FindData.ftLastWriteTime);
      Result := True;
    end;
  end;
end;

// 取文件长度
function GetFileSize(const FileName: string): Int64;
var
  FileTime: TDateTime;
begin
  Result := -1;
  GetFileInfo(FileName, Result, FileTime);
end;

// 取文件 Delphi 格式日期时间
function GetFileDateTime(const FileName: string): TDateTime;
var
  Size: Int64;
begin
  Result := 0;
  GetFileInfo(FileName, Size, Result);
end;

{$ENDIF}

// 将文件读为字符串
function LoadStringFromFile(const FileName: string): string;
begin
  try
    with TStringList.Create do
    try
      LoadFromFile(FileName);
      Result := Text;
    finally
      Free;
    end;
  except
    Result := '';
  end;
end;

// 保存字符串到为文件
function SaveStringToFile(const S, FileName: string): Boolean;
begin
  try
    with TStringList.Create do
    try
      Text := S;
      SaveToFile(FileName);
      Result := True;
    finally
      Free;
    end;
  except
    Result := False;
  end;
end;

// StringList 快排，弥补 D5、6 下排序不开放的局限
procedure QuickSortStringList(List: TStringList; L, R: Integer; SCompare: TStringListSortCompare);
var
  I, J, P: Integer;

  procedure ExchangeItems(Index1, Index2: Integer);
  var
    TempS: string;
    TempObj: TObject;
  begin
    TempS := List[Index1];
    List[Index1] := List[Index2];
    List[Index2] := TempS;

    TempObj := List.Objects[Index1];
    List.Objects[Index1] := List.Objects[Index2];
    List.Objects[Index2] := TempObj;
  end;

begin
  if (List = nil) or (List.Count = 0) then
    Exit;

  repeat
    I := L;
    J := R;
    P := (L + R) shr 1;
    repeat
      while SCompare(List, I, P) < 0 do Inc(I);
      while SCompare(List, J, P) > 0 do Dec(J);
      if I <= J then
      begin
        ExchangeItems(I, J);
        if P = I then
          P := J
        else if P = J then
          P := I;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then QuickSortStringList(List, L, J, SCompare);
    L := I;
  until I >= R;
end;

//------------------------------------------------------------------------------
// 环境变量相关
//------------------------------------------------------------------------------

procedure MultiSzToStrings(const Dest: TStrings; const Source: PChar);
var
  P: PChar;
begin
  Assert(Dest <> nil);
  Dest.Clear;
  if Source <> nil then
  begin
    P := Source;
    while P^ <> #0 do
    begin
      Dest.Add(P);
      P := StrEnd(P);
      Inc(P);
    end;
  end;
end;

{$IFDEF MSWINDOWS}

function DelEnvironmentVar(const Name: string): Boolean;
begin
  Result := SetEnvironmentVariable(PChar(Name), nil);
end;

function ExpandEnvironmentVar(var Value: string): Boolean;
var
  R: Integer;
  Expanded: string;
begin
  SetLength(Expanded, 1);
  R := ExpandEnvironmentStrings(PChar(Value), PChar(Expanded), 0);
  SetLength(Expanded, R);
  Result := ExpandEnvironmentStrings(PChar(Value), PChar(Expanded), R) <> 0;
  if Result then
  begin
    StrResetLength(Expanded);
    Value := Expanded;
  end;
end;

function GetEnvironmentVar(const Name: string; var Value: string; Expand: Boolean): Boolean;
var
  R: DWORD;
begin
  R := GetEnvironmentVariable(PChar(Name), nil, 0);
  SetLength(Value, R);
  R := GetEnvironmentVariable(PChar(Name), PChar(Value), R);
  Result := R <> 0;
  if not Result then
    Value := ''
  else
  begin
    SetLength(Value, R);
    if Expand then
      ExpandEnvironmentVar(Value);
  end;
end;

function GetEnvironmentVars(const Vars: TStrings; Expand: Boolean): Boolean;
var
  Raw: PChar;
  Expanded: string;
  I: Integer;
begin
  Vars.Clear;
  Raw := GetEnvironmentStrings;
  try
    MultiSzToStrings(Vars, Raw);
    Result := True;
  finally
    FreeEnvironmentStrings(Raw);
  end;
  if Expand then
  begin
    for I := 0 to Vars.Count - 1 do
    begin
      Expanded := Vars[I];
      if ExpandEnvironmentVar(Expanded) then
        Vars[I] := Expanded;
    end;
  end;
end;

function SetEnvironmentVar(const Name, Value: string): Boolean;
begin
  Result := SetEnvironmentVariable(PChar(Name), PChar(Value));
end;

{$ENDIF}

//------------------------------------------------------------------------------
// 扩展的字符串操作函数
//------------------------------------------------------------------------------

// 判断字符串是否可转换成浮点型
function IsFloat(const S: String): Boolean;
var
  I: Real;
  E: Integer;
begin
  Val(S, I, E);
  Result := E = 0;
  E := Trunc( I );
end;

// 判断字符串是否可转换成整型
function IsInt(const S: String): Boolean;
var
  I: Integer;
  E: Integer;
begin
  Val(S, I, E);
  Result := E = 0;
  E := Trunc( I );
end;

// 判断字符串是否可转换成 DateTime
function IsDateTime(const S: string): Boolean;
begin
  try
    StrToDateTime(S);
    Result := True;
  except
    Result := False;
  end;
end;

// 判断是否有效的邮件地址
function IsValidEmail(const S: string): Boolean;
var
  I: Integer;
  AtCount: Integer;
begin
  Result := False;
  if S = '' then Exit;
  AtCount := 0;
  for I := 1 to Length(S) do
  begin
    if S[I] = '@' then
    begin
      Inc(AtCount);
      if AtCount > 1 then
        Exit;
    end
    else if not CharInSet(S[I], ['0'..'9', 'a'..'z', 'A'..'Z', '_', '.', '-']) then
      Exit;
  end;
  Result := AtCount = 1;
end;

{$WARNINGS OFF}

// 判断是否是简单的格式化字符串，支持单引号和双引号
function IsSimpleFormat(const S: string): Boolean;
var
  T: string;
  I: Integer;
begin
  Result := False;
  T := Trim(S);
  if Length(T) <= 1 then
    Exit;

  // 去掉前后单双引号
  if (T[1] = '''') or (T[1] = '"') then
    Delete(T, 1, 1);
  if (T[Length(T)] = '''') or (T[Length(T)] = '"') then
    Delete(T, Length(T), 1);

  if T[1] <> '%' then
    Exit;

  if not (T[Length(T)] in ['A'..'Z', 'a'..'z']) then
    Exit;

  for I := 2 to Length(T) - 1 do
  begin
    if not (T[I] in ['.', '-', '0'..'9']) then
      Exit;
  end;

  Result := True;
end;

{$WARNINGS ON}

// 以不溢出的方式计算两个整型的算术平均数
function AverageNoOverflow(A, B: Integer): Integer;
begin
  Result := (A and B) + ((A xor B) shr 1);
end;

// 判断字符是否在集合内
function CharInSet(C: Char; CharSet: TAnsiCharSet): Boolean;
begin
{$IFDEF COMPILER12_UP}
  if Ord(C) <= $FF then
    Result := AnsiChar(C) in CharSet
  else
    Result := False;
{$ELSE}
  Result := C in CharSet;
{$ENDIF}
end;

// 判断s1是否包含在s2中
function InStr(const sShort: string; const sLong: string): Boolean;
var
  S1, S2: string;
begin
  S1 := LowerCase(sShort);
  S2 := LowerCase(sLong);
  Result := Pos(S1, S2) > 0;
end;

// 扩展整数转字符串函数，参数分别为目标数、长度、填充字符（默认为０）
function IntToStrEx(Value: Integer; Len: Integer; FillChar: Char = '0'): string;
begin
  Result := IntToStr(Value);
  while Length(Result) < Len do
    Result := FillChar + Result;
end;

// 带分隔符的整数－字符转换
function IntToStrSp(Value: Integer; SpLen: Integer; Sp: Char; ShowPlus: Boolean): string;
var
  S: string;
  I, J: Integer;
begin
  S := IntToStr(Value);
  if ShowPlus and (Value > 0) then
    S := '+' + S;
  Result := '';
  J := 0;
  for I := Length(S) downto 1 do
  begin
    Result := S[I] + Result;
    Inc(J);
    if ((J mod SpLen) = 0) and (I <> 1) and not CharInSet(S[I - 1], ['+', '-']) then
      Result := Sp + Result;
  end;
end;

function StrSpToInt(const Value: string; Sp: Char = ','): Int64;
begin
{$IFDEF FPC}
  Result := StrToInt64(StringReplace(Value, Sp, '', [rfReplaceAll, rfIgnoreCase]));
{$ELSE}
  Result := StrToInt64(AnsiReplaceText(Value, Sp, ''));
{$ENDIF}
end;

// 返回字符串右边的字符
function StrRight(const Str: string; Len: Integer): string;
begin
  if Len >= Length(Str) then
    Result := Str
  else
    Result := Copy(Str, Length(Str) - Len + 1, Len);
end;

// 返回字符串左边的字符
function StrLeft(const Str: string; Len: Integer): string;
begin
  if Len > Length(Str) then
    Result := Str
  else
    Result := Copy(Str, 1, Len);
end;

// 返回字符串是否以特定子串结尾
function StrEndWith(const S, Tail: string): Boolean;
begin
  Result := False;
  if (Tail <> '') and (S <> '') then
    Result := LastStrPos(Tail, S) = Length(S) - Length(Tail) + 1;
end;

// 翻转字符串
function StrReverse(const S: string): string;
var
  I: Integer;
  P: PChar;
begin
  SetLength(Result, Length(S));
  P := PChar(Result);
  for I := Length(S) downto 1 do
  begin
    P^ := S[I];
    Inc(P);
  end;
end;

// 字节转二进制串
function ByteToBin(Value: Byte): string;
const
  V: Byte = 1;
var
  I: Integer;
begin
  Result := '';
  for I := 7 downto 0 do
    if (V shl I) and Value <> 0 then
      Result := Result + '1'
    else
      Result := Result + '0';
end;

// 双字节转二进制串
function WordToBin(Value: Word): string;
const
  V: Word = 1;
var
  I: Integer;
begin
  Result := '';
  for I := 15 downto 0 do
    if (V shl I) and Value <> 0 then
      Result := Result + '1'
    else
      Result := Result + '0';
end;

// 四字节转二进制串
function DWordToBin(Value: DWORD): string;
const
  V: DWORD = 1;
var
  I: Integer;
begin
  Result := '';
  for I := 31 downto 0 do
    if (V shl I) and Value <> 0 then
      Result := Result + '1'
    else
      Result := Result + '0';
end;

// 返回字符串行
function GetLine(C: Char; Len: Integer): string;
begin
  Result := StringOfChar(C, Len);
end;

// 返回文本文件的行数
function GetTextFileLineCount(const FileName: string): Integer;
var
  Lines: TStringList;
begin
  Result := 0;
  Lines := TStringList.Create;
  try
    if FileExists(FileName) then
    begin
      Lines.LoadFromFile(FileName);
      Result := Result + Lines.Count;
    end;
  finally
    Lines.Free;
  end;
end;

// 返回空格串
function Spc(Len: Integer): string;
begin
  Result := StringOfChar(' ', Len);
end;

// 交换字串
procedure SwapStr(var S1, S2: string);
var
  T: string;
begin
  T := S1;
  S1 := S2;
  S2 := T;
end;

// 分割"非数字+数字"格式的字符串中的非数字和数字
procedure SeparateStrAndNum(const AInStr: string; var AOutStr: string;
  var AOutNum: Integer);
var
  iLen: Integer;
begin
  iLen := Length(AInStr);
  while (iLen > 0) and CharInSet(AInStr[iLen], ['0'..'9']) do Dec(iLen);
  AOutStr := Copy(AInStr, iLen + 1, MaxInt);
  if AOutStr = '' then
    AOutNum := -1
  else
    AOutNum := StrToInt(AOutStr);
  AOutStr := Copy(AInStr, 1, iLen);
end;

// 去除被引用的字符串的引用
function UnQuotedStr(const Str: string; const Ch: Char;
  const Sep: string = ''): string;
var
  First: Boolean;
  S: string;
  PS: PChar;
begin
  Result := '';
  S := Str;
  PS := PChar(S);
  First := True;

  while PS <> nil do
  begin
    PS := AnsiStrScan(PS, Ch);
    if First and (PS = nil) and (Result = '') then // 从来就没有 Quote 符的情况下
    begin
      Result := Str;
      Exit;
    end;
    First := False;

    S := AnsiExtractQuotedStr(PS, Ch);
    if (Result = '') or (S = '') then
      Result := Result + S
    else
      Result := Result + Sep + S;
  end;
end;

// 查找字符串中最后一次出现的子串位置
function LastStrPos(const SubStr, Str: string): Integer;
var
  Idx: Integer;
begin
  Result := 0;
  Idx := PosEx(SubStr, Str);
  if Idx = 0 then
    Exit;

  while Idx > 0 do
  begin
    Result := Idx;
    Idx := PosEx(SubStr, Str, Idx + 1);
  end;
end;

// 查找字符串中最后一次出现某字符的位置，无则返回 0
function LastCharPos(const S: string; C: Char): Integer;
var
  I: Integer;
begin
  I := Length(S);
  while (I > 0) and (S[I] <> C) do
    Dec(I);
  Result := I;
end;

// 查找字符串中出现的第 Counter 次的字符的位置
function CharPosWithCounter(const Sub: Char; const AStr: string;
  Counter: Integer = 1): Integer;
var
  I, J: Integer;
begin
  Result := 0;
  if Counter <= 0 then Exit;
  if AStr <> '' then
  begin
    J := 0;
    for I := 1 to Length(AStr) do
    begin
      if AStr[I] = Sub then
        Inc(J);
      if J = Counter then
      begin
        Result := I;
        Exit;
      end;
    end;
  end;
end;

function CountCharInStr(const Sub: Char; const AStr: string): Integer;
var
  I: Integer;
begin
  Result := 0;
  if AStr = '' then Exit;
  for I := 1 to Length(AStr) do
    if AStr[I] = Sub then
      Inc(Result);
end;

// 判断字符是否有效标识符字符，First 表示是否为首字符
function IsValidIdentChar(C: Char; First: Boolean): Boolean;
begin
  if First then
    Result := CharInSet(C, Alpha)
  else
    Result := CharInSet(C, AlphaNumeric);
end;

// 判断字符串是否是有效的 Unicode 标识符，只在 Unicode 下调用
function IsValidIdentW(const Ident: string): Boolean;
const
  Alpha = ['A'..'Z', 'a'..'z', '_'];
  AlphaNumeric = Alpha + ['0'..'9'];
var
  I: Integer;
begin
  Result := False;
  if (Length(Ident) = 0) or not ((AnsiChar(Ident[1]) in Alpha) or (Ord(Ident[1]) > 127)) then
    Exit;
  for I := 2 to Length(Ident) do
    if not ((AnsiChar(Ident[I]) in AlphaNumeric) or (Ord(Ident[I]) > 127)) then
      Exit;
  Result := True;
end;

// 判断宽字符串是否是有效的 Unicode 标识符，只在 BDS 以上调用
function IsValidIdentWide(const Ident: WideString): Boolean;
{$IFDEF BDS}
const
  Alpha = ['A'..'Z', 'a'..'z', '_'];
  AlphaNumeric = Alpha + ['0'..'9'];
var
  I: Integer;
{$ENDIF}
begin
  Result := False;
{$IFDEF BDS}
  if (Length(Ident) = 0) or not ((AnsiChar(Ident[1]) in Alpha) or (Ord(Ident[1]) > 127)) then
    Exit;
  for I := 2 to Length(Ident) do
    if not ((AnsiChar(Ident[I]) in AlphaNumeric) or (Ord(Ident[I]) > 127)) then
      Exit;
  Result := True;
{$ENDIF}
end;

const
  csLinesCR = #13#10;
  csStrCR = '\n';

// 多行文本转单行（换行符转'\n'）
{$IFDEF COMPILER5}
function BoolToStr(B: Boolean; UseBoolStrs: Boolean = False): string;
const
  cSimpleBoolStrs: array [boolean] of String = ('0', '-1');
begin
  if UseBoolStrs then
  begin
    if B then
      Result := 'True'
    else
      Result := 'False';
  end
  else
    Result := cSimpleBoolStrs[B];
end;
{$ENDIF COMPILER5}

function LinesToStr(const Lines: string): string;
begin
  Result := StringReplace(Lines, csLinesCR, csStrCR, [rfReplaceAll]);
end;

// 单行文本转多行（'\n'转换行符）
function StrToLines(const Str: string): string;
begin
  Result := StringReplace(Str, csStrCR, csLinesCR, [rfReplaceAll]);
end;

// 单行宽文本转多行（'\n'转换行符）
function WideStrToLines(const Str: WideString): WideString;
begin
  Result := WideStringReplace(Str, csStrCR, csLinesCR);
end;

// 日期转字符串，使用 yyyy.mm.dd 格式
function MyDateToStr(Date: TDate): string;
begin
  Result := CnDateToStr(Date);
end;

const
  csCount = 'Count';
  csItem = 'Item';

procedure ReadStringsFromIni(Ini: TCustomIniFile; const Section: string; Strings: TStrings);
var
  Count, I: Integer;
begin
  Strings.Clear;
  Count := Ini.ReadInteger(Section, csCount, 0);
  for I := 0 to Count - 1 do
    if Ini.ValueExists(Section, csItem + IntToStr(I)) then
      Strings.Add(Ini.ReadString(Section, csItem + IntToStr(I), ''));
end;

procedure WriteStringsToIni(Ini: TCustomIniFile; const Section: string; Strings: TStrings);
var
  I: Integer;
begin
  Ini.WriteInteger(Section, csCount, Strings.Count);
  for I := 0 to Strings.Count - 1 do
    Ini.WriteString(Section, csItem + IntToStr(I), Strings[I]);
end;

// 版本号转成字符串，如 $01020000 --> '1.2.0.0'
function VersionToStr(Version: DWORD): string;
begin
  Result := Format('%d.%d.%d.%d', [Version div $1000000, version mod $1000000
    div $10000, version mod $10000 div $100, version mod $100]);
end;

// 字符串转成版本号，如 '1.2.0.0' --> $01020000，如果格式不正确，返回 $01000000
function StrToVersion(const S: string): DWORD;
var
  Strs: TStrings;
begin
  try
    Strs := TStringList.Create;
    try
      Strs.Text := StringReplace(S, '.', #13#10, [rfReplaceAll]);
      if Strs.Count = 4 then
        Result := StrToInt(Strs[0]) * $1000000 + StrToInt(Strs[1]) * $10000 +
          StrToInt(Strs[2]) * $100 + StrToInt(Strs[3])
      else
        Result := $01000000;
    finally
      Strs.Free;
    end;
  except
    Result := $01000000;
  end;
end;

// 转换日期为 yyyy.mm.dd 格式字符串
function CnDateToStr(Date: TDateTime): string;
begin
  Result := FormatDateTime('yyyy.mm.dd', Date);
end;

// 将 yyyy.mm.dd 格式字符串转换为日期
function CnStrToDate(const S: string): TDateTime;
var
  I: Integer;
  Year, Month, Day: string;
begin
  try
    I := 1;
    Year := ExtractSubstr(S, I, ['.', '/', '-']);
    Month := ExtractSubstr(S, I, ['.', '/', '-']);
    Day := ExtractSubstr(S, I, ['.', '/', '-']);
    Result := EncodeDate(StrToInt(Year), StrToInt(Month), StrToInt(Day));
  except
    Result := 0;
  end;
end;

// 取日期时间的日期部分（整数）
function GetDatePart(DateTime: TDateTime): TDate;
begin
  Result := Trunc(DateTime);
end;

// 取日期时间的时间部分（小数）
function GetTimePart(DateTime: TDateTime): TTime;
begin
  Result := Frac(DateTime);
end;

// 日期时间转 '20030203132345' 式样的 14 位数字字符串
function DateTimeToFlatStr(const DateTime: TDateTime): string;
var
  Year, Month, Day, Hour, Min, Sec, MSec: Word;
begin
  DecodeDate(DateTime, Year, Month, Day);
  DecodeTime(DateTime, Hour, Min, Sec, MSec);
  Result := IntToStrEx(Year, 4) + IntToStrEx(Month, 2) + IntToStrEx(Day, 2) +
    IntToStrEx(Hour, 2) + IntToStrEx(Min, 2) + IntToStrEx(Sec, 2);
end;

// '20030203132345' 式样的 14 位数字字符串转日期时间
function FlatStrToDateTime(const Section: string; var DateTime: TDateTime): Boolean;
var
  Year, Month, Day, Hour, Min, Sec, MSec: Word;
begin
  try
    Result := False;
    if Length(Section) <> 14 then Exit;
    Year := StrToInt(Copy(Section, 1, 4));
    Month := StrToInt(Copy(Section, 5, 2));
    Day := StrToInt(Copy(Section, 7, 2));
    Hour := StrToInt(Copy(Section, 9, 2));
    Min := StrToInt(Copy(Section, 11, 2));
    Sec := StrToInt(Copy(Section, 13, 2));
    MSec := 0;
    DateTime := EncodeDate(Year, Month, Day) + EncodeTime(Hour, Min, Sec, MSec);
    Result := True;
  except
    Result := False;
  end;
end;

// 数字转大写金额
function RMBFloatToChinese(ARMBCash: Real): string;
var
  tmp1, rr: string;
  l, I, J, k: integer;
const
  n1: array[0..9] of string = ('零', '壹', '贰', '叁', '肆', '伍', '陆', '柒', '捌', '玖');
  n2: array[0..3] of string = ('', '拾', '佰', '仟');
  n3: array[0..2] of string = ('元', '万', '亿');
begin
  tmp1 := FormatFloat('#.00', ARMBCash);
  l := Length(tmp1);
  rr := '';
  if StrToInt(tmp1[l]) <> 0 then
  begin
    rr := '分';
    rr := n1[StrToInt(tmp1[l])] + rr;
  end;

  if StrToInt(tmp1[l - 1]) <> 0 then
  begin
    rr := '角' + rr;
    rr := n1[StrToInt(tmp1[l - 1])] + rr;
  end;
  I := l - 3;
  J := 0; k := 0;
  while I > 0 do
  begin
    if J mod 4 = 0 then
    begin
      rr := n3[k] + rr;
      inc(k); if k > 2 then k := 1;
      J := 0;
    end;
    if StrToInt(tmp1[I]) <> 0 then
      rr := n2[J] + rr;
    rr := n1[StrToInt(tmp1[I])] + rr;
    inc(J);
    Dec(I);
  end;
  while Pos('零零', rr) > 0 do
    rr := stringreplace(rr, '零零', '零', [rfReplaceAll]);
  rr := stringreplace(rr, '零亿', '亿零', [rfReplaceAll]);
  while Pos('零零', rr) > 0 do
    rr := stringreplace(rr, '零零', '零', [rfReplaceAll]);
  rr := stringreplace(rr, '零万', '万零', [rfReplaceAll]);
  while Pos('零零', rr) > 0 do
    rr := stringreplace(rr, '零零', '零', [rfReplaceAll]);
  rr := stringreplace(rr, '零元', '元零', [rfReplaceAll]);
  while Pos('零零', rr) > 0 do
    rr := stringreplace(rr, '零零', '零', [rfReplaceAll]);
  rr := stringreplace(rr, '亿万', '亿', [rfReplaceAll]);
  if Copy(rr, Length(rr) - 1, 2) = '零' then
    rr := Copy(rr, 1, Length(rr) - 2);
  if rr='' then
    rr:='零元';
  Result := rr;
end;

// 计算四则运算与乘方的表达式值，作者：巴哈姆特
function EvalSimpleExpression(const Value: string): Double;
var
  Code, Temp: string;
  Loop, APos: Integer;
  Opers, Consts: TStrings; // 操作符 // 操作数
  AFlag: Boolean; // 标志上一个有用的字符是否是操作符
begin
  Result:= 0;
  AFlag:= True;
  Opers:= TStringList.Create;
  Consts:= TStringList.Create;

  try
    Code:= UpperCase(Trim(Value)); // 取公式

    while Trim(Code) <> '' do
      case Code[1] of
        '+', '-', '*', '/', '^': // 如果是操作符
          begin
            if not AFlag then
            begin
              Opers.Add(Code[1]);
              Delete(Code, 1, 1);
              Temp:= '';
              AFlag:= True; // 添加了操作符以后，置标志为True
            end
            else
            begin
              Temp:= Code[1];
              Delete(Code, 1, 1);
              AFlag:= False; // 否则置标志为False
            end;
          end;

        '0'..'9', '.': // 如果是操作数
          begin
            while Trim(Code) <> '' do
              if CharInSet(Code[1], ['0'..'9', '.']) then
              begin
                Temp:= Temp + Code[1];
                Delete(Code, 1, 1);
              end
              else
                Break;

            Consts.Add(Temp);
            AFlag:= False; // 添加了操作数以后置标志为False
          end;

        '(':       // 如果带括号
          begin
            Delete(Code, 1, 1);  // 删除第一个左括号
            APos:= 1;            // 括号配对数，正数为找到的左括号比右括号多
            Temp:= '';
            while Trim(Code) <> '' do
              if (Pos(')', Code) > -1) and (APos > 0) then
              begin
                if Code[1] = '(' then // 如果找到的是左括号则记数加一
                  Inc(APos)
                else if Code[1] = ')' then // 如果找到右括号则记数减一
                  Dec(APos);

                Temp:= Temp + Code[1];
                Delete(Code, 1, 1);
              end
              else
                Break;

            Temp:= Copy(Temp, 1, Length(Temp) - 1); // 删除最后一个右括号
            Consts.Add(FloatToStr(EvalSimpleExpression(Temp))); // 递归调用函数本身优先计算括号内的值
            Temp:= '';
            AFlag:= False; // 添加括号以后置标志为False
          end;

        else // 忽略其它字符
          Delete(Code, 1, 1);
      end;

    if Opers.Count = 0 then // 如果没有操作符
    begin
      if Consts.Count > 0 then // 如果有操作数
        Result:= StrToFloat(Consts.Strings[0]);
      Exit;
    end
    else if Consts.Count = 0 then // 如果没有操作数
      Exit;

    Loop:= 0;
    while Opers.Count > 0 do
    begin
      if Opers.Strings[Loop] = '^' then // 如果操作符是乘方
      begin
        Consts.Strings[Loop]:= FloatToStr(Power(StrToFloat(Consts.Strings[Loop]), StrToFloat(Consts.Strings[Loop + 1])));
        Consts.Delete(Loop + 1);
        Opers.Delete(Loop);
        Loop:= 0;
      end
      else if Opers.IndexOf('^') > -1 then // 如果不是次方但是还有计算次方操作符
      begin
        Inc(Loop);
        Continue;
      end
      else if CharInSet(Opers.Strings[Loop][1], ['*', '/']) then // 如果是乘/除法
        case Opers.Strings[Loop][1] of
          '*':
            begin
              Consts.Strings[Loop]:= FloatToStr(StrToFloat(Consts.Strings[Loop]) * StrToFloat(Consts.Strings[Loop + 1]));
              Consts.Delete(Loop + 1);
              Opers.Delete(Loop);
              Loop:= 0;
            end;

          '/':
            begin
              Consts.Strings[Loop]:= FloatToStr(StrToFloat(Consts.Strings[Loop]) / StrToFloat(Consts.Strings[Loop + 1]));
              Consts.Delete(Loop + 1);
              Opers.Delete(Loop);
              Loop:= 0;
            end;
        end
      else if (Opers.IndexOf('*') > -1) or (Opers.IndexOf('/') > -1) then
      begin
        Inc(Loop);
        Continue;
      end
      else if CharInSet(Opers.Strings[Loop][1], ['+', '-']) then
        case Opers.Strings[Loop][1] of
          '+':
            begin
              Consts.Strings[Loop]:= FloatToStr(StrToFloat(Consts.Strings[Loop])
                + StrToFloat(Consts.Strings[Loop + 1]));
              Consts.Delete(Loop + 1);
              Opers.Delete(Loop);
              Loop:= 0;
            end;

          '-':
            begin
              Consts.Strings[Loop]:= FloatToStr(StrToFloat(Consts.Strings[Loop])
                - StrToFloat(Consts.Strings[Loop + 1]));
              Consts.Delete(Loop + 1);
              Opers.Delete(Loop);
              Loop:= 0;
            end;
        end
      else
        Inc(Loop);
    end;

    Result:= StrToFloat(Consts.Strings[0]);
  finally
    FreeAndNil(Consts);
    FreeAndNil(Opers);
  end;
end;

{$IFDEF MSWINDOWS}

// 字符串转注册表根键，支持 'HKEY_CURRENT_USER' 'HKCR' 长短两种格式
function StrToRegRoot(const S: string): HKEY;
begin
  if SameText(S, 'HKEY_CLASSES_ROOT') or SameText(S, 'HKCR') then
    Result := HKEY_CLASSES_ROOT
  else if SameText(S, 'HKEY_CURRENT_USER') or SameText(S, 'HKCU') then
    Result := HKEY_CURRENT_USER
  else if SameText(S, 'HKEY_LOCAL_MACHINE') or SameText(S, 'HKLM') then
    Result := HKEY_LOCAL_MACHINE
  else if SameText(S, 'HKEY_USERS') or SameText(S, 'HKU') then
    Result := HKEY_USERS
  else if SameText(S, 'HKEY_PERFORMANCE_DATA') or SameText(S, 'HKPD') then
    Result := HKEY_PERFORMANCE_DATA
  else if SameText(S, 'HKEY_CURRENT_CONFIG') or SameText(S, 'HKCC') then
    Result := HKEY_CURRENT_CONFIG
  else if SameText(S, 'HKEY_DYN_DATA') or SameText(S, 'HKDD') then
    Result := HKEY_DYN_DATA
  else
    Result := HKEY_CURRENT_USER;
end;

// 注册表根键转字符串，可选 'HKEY_CURRENT_USER' 'HKCR' 长短两种格式
function RegRootToStr(Key: HKEY; ShortFormat: Boolean): string;
begin
  if Key = HKEY_CLASSES_ROOT then
    if ShortFormat then
      Result := 'HKCR'
    else
      Result := 'HKEY_CLASSES_ROOT'
  else if Key = HKEY_CURRENT_USER then
    if ShortFormat then
      Result := 'HKCU'
    else
      Result := 'HKEY_CURRENT_USER'
  else if Key = HKEY_LOCAL_MACHINE then
    if ShortFormat then
      Result := 'HKLM'
    else
      Result := 'HKEY_LOCAL_MACHINE'
  else if Key = HKEY_USERS then
    if ShortFormat then
      Result := 'HKU'
    else
      Result := 'HKEY_USERS'
  else if Key = HKEY_PERFORMANCE_DATA then
    if ShortFormat then
      Result := 'HKPD'
    else
      Result := 'HKEY_PERFORMANCE_DATA'
  else if Key = HKEY_CURRENT_CONFIG then
    if ShortFormat then
      Result := 'HKCC'
    else
      Result := 'HKEY_CURRENT_CONFIG'
  else if Key = HKEY_DYN_DATA then
    if ShortFormat then
      Result := 'HKDD'
    else
      Result := 'HKEY_DYN_DATA'
  else
    Result := ''
end;

{$ENDIF}

// 从字符串中分离出子串
function ExtractSubstr(const S: string; var Pos: Integer;
  const Delims: TSysCharSet): string;
var
  I: Integer;
begin
  I := Pos;
  while (I <= Length(S)) and not CharInSet(S[I], Delims) do Inc(I);
  Result := Copy(S, Pos, I - Pos);
  if (I <= Length(S)) and CharInSet(S[I], Delims) then Inc(I);
  Pos := I;
end;

// 文件名通配符比较
function WildcardCompare(const FileWildcard, FileName: string; const IgnoreCase:
  Boolean): Boolean;

  function WildCompare(var WildS, IstS: string): Boolean;
  var
    WildPos, FilePos, l, p: Integer;
  begin
    // Start at the first wildcard/filename character
    WildPos := 1; // Wildcard position.
    FilePos := 1; // FileName position.
    while (WildPos <= Length(WildS)) do
    begin
      // '*' matches any sequence of characters.
      if WildS[WildPos] = '*' then
      begin
        // We've reached the end of the wildcard string with a * and are done.
        if WildPos = Length(WildS) then
        begin
          Result := True;
          Exit;
        end
        else
        begin
          l := WildPos + 1;
          // Anything after a * in the wildcard must match literally.
          while (l < Length(WildS)) and (WildS[l + 1] <> '*') do
            Inc(l);
          // Check for the literal match immediately after the current position.
          p := Pos(Copy(WildS, WildPos + 1, l - WildPos), IstS);
          if p > 0 then
            FilePos := p - 1
          else
          begin
            Result := False;
            Exit;
          end;
        end;
      end
      // '?' matches any character - other characters must literally match.
      else if (WildS[WildPos] <> '?') and ((Length(IstS) < WildPos) or
        (WildS[WildPos] <> IstS[FilePos])) then
      begin
        Result := False;
        Exit;
      end;
      // Match is OK so far - check the next character.
      Inc(WildPos);
      Inc(FilePos);
    end;
    Result := (FilePos > Length(IstS));
  end;

var
  NameWild, NameFile, ExtWild, ExtFile: string;
  DotPos: Integer;
begin
  // Parse to find the extension and name base of filename and wildcard.
  DotPos := LastCharPos(FileWildcard, '.');
  if DotPos = 0 then
  begin
    // Assume .* if an extension is missing
    NameWild := FileWildcard;
    ExtWild := '*';
  end
  else
  begin
    NameWild := Copy(FileWildcard, 1, DotPos - 1);
    ExtWild := Copy(FileWildcard, DotPos + 1, Length(FileWildcard));
  end;

  // We could probably modify this to use _CnExtractFileExt, etc.
  DotPos := LastCharPos(FileName, '.');
  if DotPos = 0 then
    DotPos := Length(FileName) + 1;

  NameFile := Copy(FileName, 1, DotPos - 1);
  ExtFile := Copy(FileName, DotPos + 1, Length(FileName));
  // Case insensitive check
  if IgnoreCase then
  begin
    NameWild := AnsiUpperCase(NameWild);
    NameFile := AnsiUpperCase(NameFile);
    ExtWild := AnsiUpperCase(ExtWild);
    ExtFile := AnsiUpperCase(ExtFile);
  end;
  // Both the extension and the filename must match
  Result := WildCompare(NameWild, NameFile) and WildCompare(ExtWild, ExtFile);
end;

{$IFDEF MSWINDOWS}

// 根据当前键盘布局将键盘扫描码转换成 ASCII 字符，可在 WM_KEYDOWN 等处使用
// 由于不调用 ToAscii，故可支持使用 Accent Character 的键盘布局
function ScanCodeToAscii(Code: Word): AnsiChar;
var
  I: Byte;
  C: Cardinal;
begin
  C := Code;
  if GetKeyState(VK_SHIFT) < 0 then
    C := C or $10000;
  if GetKeyState(VK_CONTROL) < 0 then
    C := C or $20000;
  if GetKeyState(VK_MENU) < 0 then
    C := C or $40000;
  for I := Low(Byte) to High(Byte) do
    if OemKeyScan(I) = C then
    begin
      Result := AnsiChar(I);
      Exit;
    end;
  Result := #0;
end;

// 返回一个虚拟键是否 Dead key
function IsDeadKey(Key: Word): Boolean;
begin
  Result := MapVirtualKey(Key, 2) and $80000000 <> 0;
end;

// 根据当前键盘状态将虚拟键转换成 ASCII 字符，可在 WM_KEYDOWN 等处使用
// 可能会导致 Accent Character 不正确
function VirtualKeyToAscii(Key: Word): AnsiChar;
var
  KeyState: TKeyboardState;
  ScanCode: Word;
  Buff: array[0..1] of AnsiChar;
begin
  Result := #0;
  if not IsDeadKey(Key) then
  begin
    case Key of
      VK_SHIFT, VK_CONTROL, VK_MENU:
        ;
    else
      begin
        ScanCode := MapVirtualKey(Key, 0);
        GetKeyboardState(KeyState);
        if ToAscii(Key, ScanCode, KeyState, @Buff, 0) = 1 then
          Result := Buff[0];
      end;
    end;
  end;
end;

// 根据当前的键盘布局将虚拟键和扫描码转换成 ASCII 字符。通过虚拟键来处理小键盘，
// 扫描码处理大键盘，支持 Accent Character 的键盘布局
function VK_ScanCodeToAscii(VKey: Word; Code: Word): AnsiChar;
begin
  if (VKey >= VK_NUMPAD0) and (VKey <= VK_DIVIDE) then
  begin
    case VKey of
      VK_NUMPAD0..VK_NUMPAD9:
        if IsNumLockDown then
          Result := AnsiChar(Ord('0') + VKey - VK_NUMPAD0)
        else
          Result := #0;
      VK_MULTIPLY: Result := '*';
      VK_ADD: Result := '+';
      VK_SEPARATOR: Result := #13;
      VK_SUBTRACT: Result := '-';
      VK_DECIMAL: Result := '.';
      VK_DIVIDE: Result := '/';
    else
      Result := #0;
    end;
  end
  else
  begin
    Result := ScanCodeToAscii(Code);
  end;
end;

// 返回当前的按键状态，暂不支持 ssDouble 状态
function GetShiftState: TShiftState;
var
  KeyState: TKeyboardState;

  function IsDown(Key: Byte): Boolean;
  begin
    Result := (Key and $80) = $80;
  end;
begin
  Result := [];
  GetKeyboardState(KeyState);
  if IsDown(KeyState[VK_LSHIFT]) or IsDown(KeyState[VK_RSHIFT]) then
    Include(Result, ssShift);
  if IsDown(KeyState[VK_LMENU]) or IsDown(KeyState[VK_RMENU]) then
    Include(Result, ssAlt);
  if IsDown(KeyState[VK_LCONTROL]) or IsDown(KeyState[VK_RCONTROL]) then
    Include(Result, ssCtrl);
  if IsDown(KeyState[VK_LBUTTON]) then
    Include(Result, ssLeft);
  if IsDown(KeyState[VK_RBUTTON]) then
    Include(Result, ssRight);
  if IsDown(KeyState[VK_MBUTTON]) then
    Include(Result, ssMiddle);
end;

// 判断当前 Shift 是否按下
function IsShiftDown: Boolean;
begin
  Result := ssShift in GetShiftState;
end;

// 判断当前 Alt 是否按下
function IsAltDown: Boolean;
begin
  Result := ssAlt in GetShiftState;
end;

// 判断当前 Ctrl 是否按下
function IsCtrlDown: Boolean;
begin
  Result := ssCtrl in GetShiftState;
end;

// 判断当前 Insert 是否按下
function IsInsertDown: Boolean;
var
  KeyState: TKeyboardState;
begin
  GetKeyboardState(KeyState);
  Result := Odd(KeyState[VK_INSERT]);
end;

// 判断当前 Caps Lock 是否按下
function IsCapsLockDown: Boolean;
var
  KeyState: TKeyboardState;
begin
  GetKeyboardState(KeyState);
  Result := Odd(KeyState[VK_CAPITAL]);
end;

// 判断当前 NumLock 是否按下
function IsNumLockDown: Boolean;
var
  KeyState: TKeyboardState;
begin
  GetKeyboardState(KeyState);
  Result := Odd(KeyState[VK_NUMLOCK]);
end;

// 判断当前 Scroll Lock 是否按下
function IsScrollLockDown: Boolean;
var
  KeyState: TKeyboardState;
begin
  GetKeyboardState(KeyState);
  Result := Odd(KeyState[VK_SCROLL]);
end;

// 使控件处理标准编辑快捷键
function HandleEditShortCut(AControl: TWinControl; AShortCut: TShortCut): Boolean;

  function SendMessageToActiveControl(Msg: Cardinal): Boolean;
  begin
    if (AControl is TCustomEdit) or (AControl is TCustomComboBox) then
    begin
      SendMessage(AControl.Handle, Msg, 0, 0);
      Result := True;
    end
    else
      Result := False;
  end;
begin
  if AControl = nil then
  begin
    Result := False;
    Exit;
  end;
  Result := True;
  if AShortCut = ShortCut(Word('C'), [ssCtrl]) then
    Result := SendMessageToActiveControl(WM_COPY)
  else if AShortCut = ShortCut(Word('X'), [ssCtrl]) then
    Result := SendMessageToActiveControl(WM_CUT)
  else if AShortCut = ShortCut(Word('V'), [ssCtrl]) then
    Result := SendMessageToActiveControl(WM_PASTE)
  else if AShortCut = ShortCut(Word('Z'), [ssCtrl]) then
    Result := SendMessageToActiveControl(WM_UNDO)
  else if AShortCut = ShortCut(Word('A'), [ssCtrl]) then
  begin
    if AControl is TCustomEdit then
      TCustomEdit(AControl).SelectAll
    else if AControl is TCustomComboBox then
      TCustomComboBox(AControl).SelectAll
    else
      Result := False;
  end
  else
    Result := False;
end;

{$ENDIF}

// 删除类名前缀 T
function RemoveClassPrefix(const ClassName: string): string;
begin
  Result := ClassName;
  if (Result <> '') and (UpperCase(Result[1]) = 'T') then
    Delete(Result, 1, 1);
end;

// 用分号分隔的作者、邮箱字符串转换为输出格式
function CnAuthorEmailToStr(Author, Email: string): string;
var
  S1, S2: string;

  function GetLeftStr(var S: string; Sep: string): string;
  var
    I: Integer;
  begin
    Result := '';
    I := AnsiPos(Sep, S);
    if I > 0 then
    begin
      Result := Trim(Copy(S, 1, I - 1));
      Delete(S, 1, I);
    end
    else begin
      Result := S;
      S := '';
    end;
  end;

begin
  Result := '';
  S1 := GetLeftStr(Author, ';');
  S2 := GetLeftStr(Email, ';');
  while S1 <> '' do
  begin
    if Result <> '' then Result := Result + #13#10;
    Result := Result + S1;
    if S2 <> '' then Result := Result + ' (' + S2 + ')';
    S1 := GetLeftStr(Author, ';');
    S2 := GetLeftStr(Email, ';');
  end;
end;

//------------------------------------------------------------------------------
// 扩展的对话框函数
//------------------------------------------------------------------------------

{$IFDEF MSWINDOWS}

// 显示提示窗口
procedure InfoDlg(const Mess: string; Caption: string; Flags: Integer);
begin
  if Caption = '' then
    Caption := SCnInformation;
  Application.MessageBox(PChar(Mess), PChar(Caption), Flags);
end;

// 显示提示确认窗口
function InfoOk(const Mess: string; Caption: string): Boolean;
begin
  if Caption = '' then
    Caption := SCnInformation;
  Result := Application.MessageBox(PChar(Mess), PChar(Caption),
    MB_OKCANCEL + MB_ICONINFORMATION) = IDOK;
end;

// 显示错误窗口
procedure ErrorDlg(const Mess: string; Caption: string);
begin
  if Caption = '' then
    Caption := SCnError;
  Application.MessageBox(PChar(Mess), PChar(Caption), MB_OK + MB_ICONSTOP);
end;

// 显示警告窗口
procedure WarningDlg(const Mess: string; Caption: string);
begin
  if Caption = '' then
    Caption := SCnWarning;
  Application.MessageBox(PChar(Mess), PChar(Caption), MB_OK + MB_ICONWARNING);
end;

// 显示查询是否窗口
function QueryDlg(const Mess: string; DefaultNo: Boolean; Caption: string): Boolean;
const
  Defaults: array[Boolean] of DWORD = (0, MB_DEFBUTTON2);
begin
  if Caption = '' then
    Caption := SCnInformation;
  Result := Application.MessageBox(PChar(Mess), PChar(Caption),
    MB_YESNO + MB_ICONQUESTION + Defaults[DefaultNo]) = IDYES;
end;

procedure LongMessageDlg(const Mess: string; AutoWrap: Boolean;
  const Caption: string);
var
  FormMess: TForm;
  mmoMess: TMemo;
  btnOK: TButton;
begin
  FormMess := TForm.Create(Application);
  try
    mmoMess := TMemo.Create(FormMess);
    btnOK := TButton.Create(FormMess);

    FormMess.Name := 'FormMess';
    FormMess.BorderStyle := bsSizeable;
    FormMess.Color := clBtnFace;
    FormMess.ClientWidth := 600;
    FormMess.ClientHeight := 500;
    FormMess.Scaled := True;
    FormMess.Font.Charset := DEFAULT_CHARSET;
    FormMess.Font.Color := clWindowText;
    FormMess.Font.Height := -11;
    FormMess.Font.Name := 'MS Sans Serif';
    FormMess.Font.Style := [];
{$IFNDEF NO_OLDCREATEORDER}
    FormMess.OldCreateOrder := False;
{$ENDIF}
    FormMess.Position := poScreenCenter;
    FormMess.PixelsPerInch := 96;

    if Caption = '' then
      FormMess.Caption := SCnInformation
    else
      FormMess.Caption := Caption;

    mmoMess.Name := 'mmoMess';
    mmoMess.Parent := FormMess;
    mmoMess.Left := 0;
    mmoMess.Top := 0;
    mmoMess.Width := 600;
    mmoMess.Height := 480;
    mmoMess.Align := alTop;
    mmoMess.Anchors := [akLeft, akTop, akRight, akBottom];
    mmoMess.ReadOnly := True;
    mmoMess.TabOrder := 0;
    mmoMess.ScrollBars := ssBoth;

    if AutoWrap then
      mmoMess.WordWrap := True;

    mmoMess.Lines.Text := Mess;

    btnOK.Name := 'btnOK';
    btnOK.Parent := FormMess;
    btnOK.Width := 75;
    btnOK.Height := 21;

    btnOK.Left := FormMess.ClientWidth - btnOK.Width - 8;
    btnOK.Top := FormMess.ClientHeight - btnOK.Height - 8;
    btnOK.Anchors := [akRight, akBottom];
    btnOK.Caption := SCnMsgDlgOK;
    btnOK.TabOrder := 1;
    btnOK.ModalResult := mrOk;

    mmoMess.Height := FormMess.ClientHeight - btnOK.Height - 16;

    FormMess.ShowModal;
  finally
    FormMess.Free;
  end;
end;

function GetAveCharSize(Canvas: TCanvas): TPoint;
var
  I: Integer;
  Buffer: array[0..51] of Char;
begin
  for I := 0 to 25 do Buffer[I] := Chr(I + Ord('A'));
  for I := 0 to 25 do Buffer[I + 26] := Chr(I + Ord('a'));
  GetTextExtentPoint(Canvas.Handle, Buffer, 52, TSize(Result));
  Result.X := Result.X div 52;
end;

function MultiButtonsDlg(const Mess: string; Buttons: TCnDlgButtonCaptions;
  Caption: string = ''): TCnDlgResult;
var
  Form: TForm;
  Prompt: TLabel;
  DialogUnits: TPoint;
  ButtonTop, ButtonWidth, ButtonHeight, ButtonGap: Integer;
  ButtonsVal, BC, I, J, W: Integer;
  ButtonSet: TIntegerSet;
  Btn: TButton;
{$IFDEF CREATE_PARAMS_BUG}
  OldLong: Longint;
  AHandle: THandle;
  NeedChange: Boolean;
{$ENDIF}
begin
  Result := cdrOK;
  if Caption = '' then
    Caption := SCnInformation;

  if Buttons = [] then
    Buttons := [cdbOK];

  ButtonsVal := 0;
  Move(Buttons, ButtonsVal, SizeOf(Buttons));
  BC := GetSetElementCount(Buttons, SizeOf(Buttons));
  Integer(ButtonSet) := ButtonsVal;

  // 共有 BC 个按钮，要根据这个数字设置窗体宽度
{$IFDEF CREATE_PARAMS_BUG}
  NeedChange := False;
  OldLong := 0;
  AHandle := Application.ActiveFormHandle;
{$ENDIF}

  Form := TForm.Create(Application);
  try
    Form.Scaled := False;
    Form.Font.Handle := GetStockObject(DEFAULT_GUI_FONT);
    Form.Canvas.Font := Form.Font;
    DialogUnits := GetAveCharSize(Form.Canvas);
    Form.BorderStyle := bsDialog;
    Form.Caption := Caption;

    ButtonTop := MulDiv(41, DialogUnits.Y, 8);
    ButtonWidth := MulDiv(50, DialogUnits.X, 4);
    ButtonHeight := MulDiv(14, DialogUnits.Y, 8);
    ButtonGap := MulDiv(6, DialogUnits.X, 4);

    // 计算文字宽度加上左右边距
    W := Form.Canvas.TextWidth(Mess) + 2 * MulDiv(8, DialogUnits.X, 4);

    // 计算 ClientWidth
    if BC > 2 then
      Form.ClientWidth := MulDiv(180, DialogUnits.X, 4)
        + (ButtonWidth + ButtonGap) * (BC - 2)
    else
      Form.ClientWidth := MulDiv(180, DialogUnits.X, 4);

    if Form.ClientWidth < W then // 如果文字超宽就需要放宽窗体
      Form.ClientWidth := W;

    Form.ClientHeight := MulDiv(64, DialogUnits.Y, 8);
    Form.Position := poScreenCenter;

    // 创建提示文字
    Prompt := TLabel.Create(Form);
    Prompt.Parent := Form;
    Prompt.AutoSize := True;
    Prompt.Left := MulDiv(8, DialogUnits.X, 4);
    Prompt.Top := MulDiv(8, DialogUnits.Y, 8);
    Prompt.Caption := Mess;

    // 循环创建按钮
    J := 0;
    for I := 0 to SizeOf(Integer) * 8 - 1 do
    begin
      if I in ButtonSet then
      begin
        Inc(J);
        Btn := TButton.Create(Form);
        Btn.Parent := Form;
        Btn.Caption := SCnDlgButtonCaptions[TCnDlgButtonCaption(I)]^;
        Btn.ModalResult := I + 1;

        // Width - 右边距 - (Gap + ButtonWidth) * J
        Btn.SetBounds(Form.Width - MulDiv(8, DialogUnits.X, 4) -
          (ButtonGap + ButtonWidth) * J, ButtonTop, ButtonWidth, ButtonHeight);
      end;
    end;

{$IFDEF CREATE_PARAMS_BUG}
    if AHandle <> 0 then
    begin
      OldLong := GetWindowLong(AHandle, GWL_EXSTYLE);
      NeedChange := OldLong and WS_EX_TOOLWINDOW = WS_EX_TOOLWINDOW;
      if NeedChange then
        SetWindowLong(AHandle, GWL_EXSTYLE, OldLong and not WS_EX_TOOLWINDOW);
    end;
{$ENDIF}

    J := Form.ShowModal;
    if J > 0 then
      Result := TCnDlgResult(J - 1);
  finally
{$IFDEF CREATE_PARAMS_BUG}
    if NeedChange and (OldLong <> 0) then
      SetWindowLong(AHandle, GWL_EXSTYLE, OldLong);
{$ENDIF}
    Form.Free;
  end;
end;

// 输入对话框
function CnInputQuery(const ACaption, APrompt: string;
  var Value: string; Ini: TCustomIniFile; const Section: string;
  APassword: Boolean; FormCallBack: TCnSenderCallback): Boolean;
var
  Form: TForm;
  Prompt: TLabel;
  Edit: TEdit;
  ComboBox: TComboBox;
  DialogUnits: TPoint;
  ButtonTop, ButtonWidth, ButtonHeight: Integer;
{$IFDEF CREATE_PARAMS_BUG}
  OldLong: Longint;
  AHandle: THandle;
  NeedChange: Boolean;
{$ENDIF}
begin
  Result := False;
  Edit := nil;
  ComboBox := nil;

{$IFDEF CREATE_PARAMS_BUG}
  NeedChange := False;
  OldLong := 0;
  AHandle := Application.ActiveFormHandle;
{$ENDIF}

  Form := TForm.Create(Application);
  with Form do
    try
      Scaled := False;
      Font.Handle := GetStockObject(DEFAULT_GUI_FONT);
      Canvas.Font := Font;
      DialogUnits := GetAveCharSize(Canvas);
      BorderStyle := bsDialog;
      Caption := ACaption;
      ClientWidth := MulDiv(180, DialogUnits.X, 4);
      ClientHeight := MulDiv(63, DialogUnits.Y, 8);
      Position := poScreenCenter;

      Prompt := TLabel.Create(Form);
      with Prompt do
      begin
        Parent := Form;
        AutoSize := True;
        Left := MulDiv(8, DialogUnits.X, 4);
        Top := MulDiv(8, DialogUnits.Y, 8);
        Caption := APrompt;
      end;

      if Assigned(Ini) then
      begin
        ComboBox := TComboBox.Create(Form);
        with ComboBox do
        begin
          Parent := Form;
          Left := Prompt.Left;
          Top := MulDiv(19, DialogUnits.Y, 8);
          Width := MulDiv(164, DialogUnits.X, 4);
          // MaxLength := 1024;
          ReadStringsFromIni(Ini, Section, ComboBox.Items);
          if (Value = '') and (ComboBox.Items.Count > 0) then
            Text := ComboBox.Items[0]
          else
            Text := Value;
          SelectAll;
        end;
      end
      else
      begin
        Edit := TEdit.Create(Form);
        with Edit do
        begin
          Parent := Form;
          Left := Prompt.Left;
          Top := MulDiv(19, DialogUnits.Y, 8);
          Width := MulDiv(164, DialogUnits.X, 4);
          // MaxLength := 1024;
          if APassword then
            PasswordChar := '*';
          Text := Value;
          SelectAll;
        end;
      end;

      ButtonTop := MulDiv(41, DialogUnits.Y, 8);
      ButtonWidth := MulDiv(50, DialogUnits.X, 4);
      ButtonHeight := MulDiv(14, DialogUnits.Y, 8);

      with TButton.Create(Form) do
      begin
        Parent := Form;
        Caption := SCnMsgDlgOK;
        ModalResult := mrOk;
        Default := True;
        SetBounds(MulDiv(38, DialogUnits.X, 4), ButtonTop, ButtonWidth,
          ButtonHeight);
      end;

      with TButton.Create(Form) do
      begin
        Parent := Form;
        Caption := SCnMsgDlgCancel;
        ModalResult := mrCancel;
        Cancel := True;
        SetBounds(MulDiv(92, DialogUnits.X, 4), ButtonTop, ButtonWidth,
          ButtonHeight);
      end;

{$IFDEF CREATE_PARAMS_BUG}
      if AHandle <> 0 then
      begin
        OldLong := GetWindowLong(AHandle, GWL_EXSTYLE);
        NeedChange := OldLong and WS_EX_TOOLWINDOW = WS_EX_TOOLWINDOW;
        if NeedChange then
          SetWindowLong(AHandle, GWL_EXSTYLE, OldLong and not WS_EX_TOOLWINDOW);
      end;
{$ENDIF}

      if Assigned(FormCallBack) then // 给外界一个处理界面的机会，如 HDPI 模式下放大等
        FormCallBack(Form);

      if ShowModal = mrOk then
      begin
        if Assigned(ComboBox) then
        begin
          Value := ComboBox.Text;
          AddComboBoxTextToItems(ComboBox);
          WriteStringsToIni(Ini, Section, ComboBox.Items);
        end
        else
          Value := Edit.Text;
        Result := True;
      end;
    finally
{$IFDEF CREATE_PARAMS_BUG}
      if NeedChange and (OldLong <> 0) then
        SetWindowLong(AHandle, GWL_EXSTYLE, OldLong);
{$ENDIF}
      Form.Free;
    end;
end;

{$ENDIF}

// 输入单行字符串的对话框，注意 FMX 下历史功能与回调无效，仅简单返回内容
function CnInputBox(const ACaption, APrompt, ADefault: string;
  Ini: TCustomIniFile; const Section: string;
  FormCallBack: TCnSenderCallback): string;
begin
{$IFDEF MSWINDOWS}
  Result := ADefault;
  if not CnInputQuery(ACaption, APrompt, Result, Ini, Section, False, FormCallBack) then
    Result := '';
{$ELSE}
  Result := InputBox(ACaption, APrompt, ADefault);
{$ENDIF}
end;

{$IFDEF MSWINDOWS}

// 输入多行字符串的对话框，返回 True 表示用户输入后点击 OK 了，输入内容在 Value 中
function CnInputMultiLineQuery(const ACaption, APrompt: string;
  var Value: string; FormCallBack: TCnSenderCallback): Boolean;
var
  Form: TForm;
  Prompt: TLabel;
  Memo: TMemo;
  DialogUnits: TPoint;
  ButtonTop, ButtonWidth, ButtonHeight: Integer;
{$IFDEF CREATE_PARAMS_BUG}
  OldLong: Longint;
  AHandle: THandle;
  NeedChange: Boolean;
{$ENDIF}
begin
  Result := False;

{$IFDEF CREATE_PARAMS_BUG}
  NeedChange := False;
  OldLong := 0;
  AHandle := Application.ActiveFormHandle;
{$ENDIF}

  Form := TForm.Create(Application);
  with Form do
  try
    Scaled := False;
    Font.Handle := GetStockObject(DEFAULT_GUI_FONT);
    Canvas.Font := Font;
    DialogUnits := GetAveCharSize(Canvas);
    BorderStyle := bsDialog;
    Caption := ACaption;
    ClientWidth := MulDiv(360, DialogUnits.X, 4);
    ClientHeight := MulDiv(128, DialogUnits.Y, 8);
    Position := poScreenCenter;

    Prompt := TLabel.Create(Form);
    with Prompt do
    begin
      Parent := Form;
      AutoSize := True;
      Left := MulDiv(8, DialogUnits.X, 4);
      Top := MulDiv(8, DialogUnits.Y, 8);
      Caption := APrompt;
    end;

    Memo := TMemo.Create(Form);
    with Memo do
    begin
      Parent := Form;
      Left := Prompt.Left;
      Top := MulDiv(19, DialogUnits.Y, 8);
      Width := MulDiv(344, DialogUnits.X, 4);
      Height := MulDiv(80, DialogUnits.Y, 8);
      ScrollBars := ssBoth;

      Text := Value;
      SelectAll;
    end;

    ButtonTop := MulDiv(108, DialogUnits.Y, 8);
    ButtonWidth := MulDiv(50, DialogUnits.X, 4);
    ButtonHeight := MulDiv(14, DialogUnits.Y, 8);

    with TButton.Create(Form) do
    begin
      Parent := Form;
      Caption := SCnMsgDlgOK;
      ModalResult := mrOk;
      Default := True;
      SetBounds(MulDiv(130, DialogUnits.X, 4), ButtonTop, ButtonWidth,
        ButtonHeight);
    end;

    with TButton.Create(Form) do
    begin
      Parent := Form;
      Caption := SCnMsgDlgCancel;
      ModalResult := mrCancel;
      Cancel := True;
      SetBounds(MulDiv(185, DialogUnits.X, 4), ButtonTop, ButtonWidth,
        ButtonHeight);
    end;

{$IFDEF CREATE_PARAMS_BUG}
    if AHandle <> 0 then
    begin
      OldLong := GetWindowLong(AHandle, GWL_EXSTYLE);
      NeedChange := OldLong and WS_EX_TOOLWINDOW = WS_EX_TOOLWINDOW;
      if NeedChange then
        SetWindowLong(AHandle, GWL_EXSTYLE, OldLong and not WS_EX_TOOLWINDOW);
    end;
{$ENDIF}

    if Assigned(FormCallBack) then // 给外界一个处理界面的机会，如 HDPI 模式下放大等
      FormCallBack(Form);

    if ShowModal = mrOk then
    begin
      Value := Memo.Text;
      Result := True;
    end;
  finally
{$IFDEF CREATE_PARAMS_BUG}
    if NeedChange and (OldLong <> 0) then
      SetWindowLong(AHandle, GWL_EXSTYLE, OldLong);
{$ENDIF}
    Form.Free;
  end;
end;

// 输入多行字符串的对话框，用户输入内容为空或 Cancel 时返回空字符串
function CnInputMultiLineBox(const ACaption, APrompt, ADefault: string;
  FormCallBack: TCnSenderCallback = nil): string;
begin
  Result := ADefault;
  if not CnInputMultiLineQuery(ACaption, APrompt, Result, FormCallBack) then
    Result := '';
end;

procedure CnShowHexData(Data: Pointer; DataByteLength: Integer; BaseAddr: Integer;
  const ACaption: string; Modal: Boolean);
var
  F: TForm;
  H: TCnHexEditor;
begin
  F := TForm.Create(Application);
  with F do
  begin
    Width := Screen.Width div 2;
    Height := Screen.Height div 2;
    Position := poScreenCenter;

    if ACaption = '' then
      Caption := SCnPackAbout
    else
      Caption := ACaption;

    H := TCnHexEditor.Create(F);
    H.Parent := F;

    H.Font.Name := 'FixedSys';
    H.Align := alClient;
    H.LoadFromBuffer(Data^, DataByteLength);

    if BaseAddr > 0 then
      H.BaseAddress := BaseAddr;

    if Modal then
    begin
      try
        ShowModal;
      finally
        Free;
      end;
    end
    else
      Show; // 显示出来后供外部关闭释放
  end;
end;

function CnSingleOptionQuery(const ACaption, APrompt: string; Options: TStrings;
  FormCallBack: TCnSenderCallback): Integer;
var
  Form: TForm;
  RG: TRadioGroup;
  DialogUnits: TPoint;
  ButtonTop, ButtonWidth, ButtonHeight: Integer;
{$IFDEF CREATE_PARAMS_BUG}
  OldLong: Longint;
  AHandle: THandle;
  NeedChange: Boolean;
{$ENDIF}
begin
  Result := -1;
  if (Options = nil) or (Options.Count <= 0) then
    Exit;

{$IFDEF CREATE_PARAMS_BUG}
  NeedChange := False;
  OldLong := 0;
  AHandle := Application.ActiveFormHandle;
{$ENDIF}

  Form := TForm.Create(Application);
  with Form do
  try
    Scaled := False;
    Font.Handle := GetStockObject(DEFAULT_GUI_FONT);
    Canvas.Font := Font;
    DialogUnits := GetAveCharSize(Canvas);
    BorderStyle := bsDialog;
    Caption := ACaption;
    if Caption = '' then
      Caption := SCnPackAbout;

    ClientWidth := MulDiv(360, DialogUnits.X, 4);
    ClientHeight := MulDiv(128, DialogUnits.Y, 8);
    Position := poScreenCenter;

    RG := TRadioGroup.Create(Form);
    with RG do
    begin
      Parent := Form;

      Left := MulDiv(8, DialogUnits.X, 4);
      Top := MulDiv(8, DialogUnits.Y, 8);
      Caption := APrompt;
      if Caption = '' then
        Caption := Form.Caption;

      Items.Assign(Options);
    end;

    ButtonTop := MulDiv(108, DialogUnits.Y, 8);
    ButtonWidth := MulDiv(50, DialogUnits.X, 4);
    ButtonHeight := MulDiv(14, DialogUnits.Y, 8);

    with TButton.Create(Form) do
    begin
      Parent := Form;
      Caption := SCnMsgDlgOK;
      ModalResult := mrOk;
      Default := True;
      SetBounds(MulDiv(130, DialogUnits.X, 4), ButtonTop, ButtonWidth,
        ButtonHeight);
    end;

    with TButton.Create(Form) do
    begin
      Parent := Form;
      Caption := SCnMsgDlgCancel;
      ModalResult := mrCancel;
      Cancel := True;
      SetBounds(MulDiv(185, DialogUnits.X, 4), ButtonTop, ButtonWidth,
        ButtonHeight);
    end;

{$IFDEF CREATE_PARAMS_BUG}
    if AHandle <> 0 then
    begin
      OldLong := GetWindowLong(AHandle, GWL_EXSTYLE);
      NeedChange := OldLong and WS_EX_TOOLWINDOW = WS_EX_TOOLWINDOW;
      if NeedChange then
        SetWindowLong(AHandle, GWL_EXSTYLE, OldLong and not WS_EX_TOOLWINDOW);
    end;
{$ENDIF}

    if Assigned(FormCallBack) then // 给外界一个处理界面的机会，如 HDPI 模式下放大等
      FormCallBack(Form);

    if ShowModal = mrOk then
      Result := RG.ItemIndex;
  finally
{$IFDEF CREATE_PARAMS_BUG}
    if NeedChange and (OldLong <> 0) then
      SetWindowLong(AHandle, GWL_EXSTYLE, OldLong);
{$ENDIF}
    Form.Free;
  end;
end;

{$ENDIF}

//------------------------------------------------------------------------------
// 位扩展日期时间操作函数
//------------------------------------------------------------------------------

function GetYear(Date: TDate): Integer;
var
  y, m, d: WORD;
begin
  DecodeDate(Date, y, m, d);
  Result := y;
end;

function GetMonth(Date: TDate): Integer;
var
  y, m, d: WORD;
begin
  DecodeDate(Date, y, m, d);
  Result := m;
end;

function GetDay(Date: TDate): Integer;
var
  y, m, d: WORD;
begin
  DecodeDate(Date, y, m, d);
  Result := d;
end;

function GetHour(Time: TTime): Integer;
var
  h, m, s, ms: WORD;
begin
  DecodeTime(Time, h, m, s, ms);
  Result := h;
end;

function GetMinute(Time: TTime): Integer;
var
  h, m, s, ms: WORD;
begin
  DecodeTime(Time, h, m, s, ms);
  Result := m;
end;

function GetSecond(Time: TTime): Integer;
var
  h, m, s, ms: WORD;
begin
  DecodeTime(Time, h, m, s, ms);
  Result := s;
end;

function GetMSecond(Time: TTime): Integer;
var
  h, m, s, ms: WORD;
begin
  DecodeTime(Time, h, m, s, ms);
  Result := ms;
end;

//------------------------------------------------------------------------------
// 位操作函数
//------------------------------------------------------------------------------

// 设置位
procedure SetBit(var Value: Byte; Bit: TByteBit; IsSet: Boolean);
begin
  if IsSet then
    Value := Value or (1 shl Bit)
  else
    Value := Value and not (1 shl Bit);
end;

procedure SetBit(var Value: WORD; Bit: TWordBit; IsSet: Boolean);
begin
  if IsSet then
    Value := Value or (1 shl Bit)
  else
    Value := Value and not (1 shl Bit);
end;

procedure SetBit(var Value: DWORD; Bit: TDWordBit; IsSet: Boolean);
begin
  if IsSet then
    Value := Value or (1 shl Bit)
  else
    Value := Value and not (1 shl Bit);
end;

// 取位
function GetBit(Value: Byte; Bit: TByteBit): Boolean;
begin
  Result := Value and (1 shl Bit) <> 0;
end;

function GetBit(Value: WORD; Bit: TWordBit): Boolean;
begin
  Result := Value and (1 shl Bit) <> 0;
end;

function GetBit(Value: DWORD; Bit: TDWordBit): Boolean;
begin
  Result := Value and (1 shl Bit) <> 0;
end;

function CountSetBits(const Value: Cardinal): Integer;
var
  LS: DWORD;
  BT: Int64;
  I: DWORD;
begin
  LS := SizeOf(Cardinal) * 8 - 1;
  Result := 0;
  BT := 1 shl LS;

  for I := 0 to LS do
  begin
    if (Value and BT) <> 0 then
      Inc(Result);
    BT := BT shr 1;
  end;
end;

//------------------------------------------------------------------------------
// 系统功能函数
//------------------------------------------------------------------------------

{$IFDEF MSWINDOWS}

// 移动鼠标到控件
procedure MoveMouseIntoControl(AWinControl: TControl);
var
  rtControl: TRect;
begin
  rtControl := AWinControl.BoundsRect;
  MapWindowPoints(AWinControl.Parent.Handle, 0, rtControl, 2);
  SetCursorPos(rtControl.Left + (rtControl.Right - rtControl.Left) div 2,
    rtControl.Top + (rtControl.Bottom - rtControl.Top) div 2);
end;

// 将 ComboBox 的文本内容增加到下拉列表中
procedure AddComboBoxTextToItems(ComboBox: TComboBox; MaxItemsCount: Integer = 10);
var
  Text: string;
begin
  if ComboBox.Text <> '' then
  begin
    Text := ComboBox.Text;
    if ComboBox.Items.IndexOf(ComboBox.Text) < 0 then
      ComboBox.Items.Insert(0, ComboBox.Text)
    else
      ComboBox.Items.Move(ComboBox.Items.IndexOf(ComboBox.Text), 0);
    while (MaxItemsCount > 1) and (ComboBox.Items.Count > MaxItemsCount) do
      ComboBox.Items.Delete(ComboBox.Items.Count - 1);
    ComboBox.Text := Text;
  end;
end;

// 动态设置分辨率
function DynamicResolution(x, y: WORD): Boolean;
var
  lpDevMode: TDeviceMode;
begin
  Result := EnumDisplaySettings(nil, 0, lpDevMode);
  if Result then
  begin
    lpDevMode.dmFields := DM_PELSWIDTH or DM_PELSHEIGHT;
    lpDevMode.dmPelsWidth := x;
    lpDevMode.dmPelsHeight := y;
    Result := ChangeDisplaySettings(lpDevMode, 0) = DISP_CHANGE_SUCCESSFUL;
  end;
end;

// 窗口最上方显示
procedure StayOnTop(Handle: HWND; OnTop: Boolean);
const
  csOnTop: array[Boolean] of HWND = (HWND_NOTOPMOST, HWND_TOPMOST);
begin
  SetWindowPos(Handle, csOnTop[OnTop], 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE or
    SWP_NOACTIVATE);
end;

var
  WndLong: Integer;

// 设置程序是否出现在任务栏
procedure SetHidden(Hide: Boolean);
begin
  ShowWindow(Application.Handle, SW_HIDE);
  if Hide then
    SetWindowLong(Application.Handle, GWL_EXSTYLE,
      WndLong or WS_EX_TOOLWINDOW and not WS_EX_APPWINDOW or WS_EX_TOPMOST)
  else
    SetWindowLong(Application.Handle, GWL_EXSTYLE, WndLong);
  ShowWindow(Application.Handle, SW_SHOW);
end;

const
  csWndShowFlag: array[Boolean] of DWORD = (SW_HIDE, SW_RESTORE);

// 设置任务栏是否可见
procedure SetTaskBarVisible(Visible: Boolean);
var
  wndHandle: THandle;
begin
  wndHandle := FindWindow('Shell_TrayWnd', nil);
  ShowWindow(wndHandle, csWndShowFlag[Visible]);
end;

// 设置桌面是否可见
procedure SetDesktopVisible(Visible: Boolean);
var
  hDesktop: THandle;
begin
  hDesktop := FindWindow('Progman', nil);
  ShowWindow(hDesktop, csWndShowFlag[Visible]);
end;

type
  TSetLayeredWindowAttributes = function (Hwnd: THandle; crKey: COLORREF;
    bAlpha: Byte; dwFlags: DWORD): BOOL; stdcall;
  TChangeWindowMessageFilter = function (Message: UINT; dwFlag: DWORD): BOOL; stdcall;
  TChangeWindowMessageFilterEx = function (HWnd: THandle; Message: UINT; action: DWORD; Dummy: Pointer): BOOL; stdcall;

var
  SetLayeredWindowAttributesProc: TSetLayeredWindowAttributes = nil;
  ChangeWindowMessageFilterProc: TChangeWindowMessageFilter = nil;
  ChangeWindowMessageFilterExProc: TChangeWindowMessageFilterEx = nil;

procedure InitAPIs;
const
  sUser32 = 'User32.dll';
var
  ModH: HMODULE;
begin
  ModH := GetModuleHandle(sUser32);
  if ModH <> 0 then
  begin
     @SetLayeredWindowAttributesProc := GetProcAddress(ModH, 'SetLayeredWindowAttributes');
     @ChangeWindowMessageFilterProc := GetProcAddress(ModH, 'ChangeWindowMessageFilter');
     @ChangeWindowMessageFilterExProc := GetProcAddress(ModH, 'ChangeWindowMessageFilterEx');
  end;
end;

// 设置窗体 Alpha 透明值
function CnSetWindowAlphaBlend(Hwnd: THandle; AlphaBlendValue: Byte): Boolean;
const
  WS_EX_LAYERED = $00080000;
  LWA_ALPHA = $00000002;
var
  AStyle: Integer;
begin
  if Assigned(SetLayeredWindowAttributesProc) then
  begin
    AStyle := GetWindowLong(Hwnd, GWL_EXSTYLE);
    if (AStyle and WS_EX_LAYERED) = 0 then
      SetWindowLong(Hwnd, GWL_EXSTYLE, AStyle or WS_EX_LAYERED);
    Result := SetLayeredWindowAttributesProc(Hwnd, 0, AlphaBlendValue, LWA_ALPHA);
  end
  else
    Result := False;
end;

// 强制让一个窗口显示在前台
function ForceForegroundWindow(HWND: HWND): Boolean;
var
  ThreadID1, ThreadID2: DWORD;
begin
  if HWND = GetForegroundWindow then
    Result := True
  else
  begin
    ThreadID1 := GetWindowThreadProcessId(GetForegroundWindow, nil);
    ThreadID2 := GetWindowThreadProcessId(HWND, nil);
    if ThreadID1 <> ThreadID2 then
    begin
      AttachThreadInput(ThreadID1, ThreadID2, True);
      Result := SetForegroundWindow(HWND);
      AttachThreadInput(ThreadID1, ThreadID2, False);
    end
    else
      Result := SetForegroundWindow(HWND);
    if IsIconic(HWND) then
      ShowWindow(HWND, SW_RESTORE)
    else
      ShowWindow(HWND, SW_SHOW);
  end;
end;

// Windows 10 下调用 ChangeWindowMessageFilter 设置允许接收低权限进程的拖拽等消息
function AllowGlobalFileDragMessage: Boolean;
begin
  Result := False;
  if CheckWin10 and Assigned(ChangeWindowMessageFilterProc) then
  begin
    ChangeWindowMessageFilterProc(WM_COPYGLOBALDATA, MSGFLT_ADD);
    ChangeWindowMessageFilterProc(WM_COPYDATA, MSGFLT_ADD);
    ChangeWindowMessageFilterProc(WM_DROPFILES, MSGFLT_ADD);
    Result := True;
  end;
end;

// Windows 10 下调用 ChangeWindowMessageFilterEx 设置允许窗口接收低权限进程的拖拽等消息
function AllowWindowFileDragMessage(HWnd: THandle): Boolean;
begin
  Result := False;
  if CheckWin10 and Assigned(ChangeWindowMessageFilterExProc) then
  begin
    ChangeWindowMessageFilterExProc(HWnd, WM_COPYGLOBALDATA, MSGFLT_ADD, nil);
    ChangeWindowMessageFilterExProc(HWnd, WM_COPYDATA, MSGFLT_ADD, nil);
    ChangeWindowMessageFilterExProc(HWnd, WM_DROPFILES, MSGFLT_ADD, nil);
    Result := True;
  end;
end;

// 取桌面区域
function GetWorkRect(const Form: TCustomForm = nil): TRect;
var
  Monitor: TMonitor;
  MonInfo: TMonitorInfo;
begin
  Result.Top := 0;
  Result.Left := 0;
  Result.Right := Screen.Width;
  Result.Bottom := Screen.Height;
  if Assigned(Form) then
  begin
    Monitor := Form.Monitor;
    if Assigned(Monitor) then
    begin
      MonInfo.cbSize := SizeOf(MonInfo);
      GetMonitorInfo(Monitor.Handle, @MonInfo);
      Result := MonInfo.rcWork;
    end;
  end
  else
    SystemParametersInfo(SPI_GETWORKAREA, 0, @Result, 0);
end;

// 显示等待光标
procedure BeginWait;
begin
  Screen.Cursor := crHourGlass;
end;

// 结束等待光标
procedure EndWait;
begin
  Screen.Cursor := crDefault;
end;

// 检测是否 Win95/98 平台
function CheckWindows9598: Boolean;
var
  V: TOSVersionInfo;
begin
  V.dwOSVersionInfoSize := SizeOf(V);
  Result := False;
  if not GetVersionEx(V) then Exit;
  if V.dwPlatformId = VER_PLATFORM_WIN32_WINDOWS then
    Result := True;
end;

// 检测是否 WinNT 平台
function CheckWindowsNT: Boolean;
var
  V: TOSVersionInfo;
begin
  V.dwOSVersionInfoSize := SizeOf(V);
  Result := False;
  if not GetVersionEx(V) then Exit;
  if (V.dwPlatformId = VER_PLATFORM_WIN32_NT) and ((V.dwMajorVersion = 3) or (V.dwMajorVersion = 4)) then
    Result := True;
end;

// 检测是否 Windows XP 或以上平台
function CheckWinXP: Boolean;
begin
  Result := (Win32MajorVersion > 5) or
    ((Win32MajorVersion = 5) and (Win32MinorVersion >= 1));
end;

// 检查是否 Vista/Windows 7 或以上系统
function CheckWinVista: Boolean;
begin
  Result := Win32MajorVersion >= 6;
end;

// 检查是否 Windows 8 或以上系统
function CheckWin8: Boolean;
begin
  Result := (Win32MajorVersion >= 6) and (Win32MinorVersion >= 2);
end;

// 检查是否 Windows 10 或以上系统
function CheckWin10: Boolean;
begin
  // 注意 Windows 10 下如果 Manifest 中不写支持 Windows 10 的话，Win32MajorVersion 是 6。
  Result := Win32MajorVersion >= 10;
end;

// 检查是否是 64 位 Windows
function CheckWin64: Boolean;
type
  TGetNativeSystemInfoProc = procedure(var lpSystemInfo: TSystemInfo); stdcall;
const
  PROCESSOR_ARCHITECTURE_AMD64 = 9;
  PROCESSOR_ARCHITECTURE_IA64 = 6;
var
  Proc: TGetNativeSystemInfoProc;
  Info: TSystemInfo;
begin
  Result := False;
  Proc := TGetNativeSystemInfoProc(GetProcAddress(GetModuleHandle('kernel32'), 'GetNativeSystemInfo'));
  if Assigned(Proc) then
  begin
    Proc(Info);
    Result := (Info.wProcessorArchitecture = PROCESSOR_ARCHITECTURE_AMD64) or
      (Info.wProcessorArchitecture = PROCESSOR_ARCHITECTURE_IA64);
  end;
end;

// 检查当前进程是否是 32 位进程跑在 64 位子系统里
function CheckWow64: Boolean;
begin
  Result := CheckProcess64(GetCurrentProcess);
end;

// 检查指定进程是否 64 位，参数为进程句柄（并非进程号）如传 0 ，则判断当前进程
function CheckProcess64(ProcessHandle: THandle = 0): Boolean;
begin
  Result := False;
  if not CheckWin64 then  // 32 位系统下只有 32 位进程
    Exit;

  if ProcessHandle = 0 then
    ProcessHandle := GetCurrentProcess;

  Result := not CheckProcessWow64(ProcessHandle); // 64 位下要判断是否 32 位跑 64 位子系统，是则 32
end;

// 检查指定进程是否是 32 位进程跑在 64 位子系统里。参数为进程句柄（并非进程号）
function CheckProcessWow64(ProcessHandle: THandle): Boolean;
type
  TIsWow64ProcessProc = function(Handle: THandle; var IsWow64: LongBool): LongBool stdcall;
var
  Proc: TIsWow64ProcessProc;
  IsWow64: LongBool;
begin
  Result := False;
  Proc := TIsWow64ProcessProc(GetProcAddress(GetModuleHandle('kernel32'), 'IsWow64Process'));
  if Assigned(Proc) then
  begin
    if Proc(ProcessHandle, IsWow64) then
      Result := IsWow64;
  end;
end;

// 检查系统和当前进程是否支持 XP Manifest
function CheckXPManifest(var OSSupport, AppValid: Boolean): Boolean;
var
  hProc: THandle;
  hMods: array[0..1023] of HMODULE;
  Name: array[0..1023] of AnsiChar;
  cbNeeded: DWORD;
  I: Integer;
  Ver: TVersionNumber;
begin
  Result := False;
  OSSupport := False;
  AppValid := True;
  hProc := GetCurrentProcess;
  EnumProcessModules(hProc, @hMods, SizeOf(hMods), cbNeeded);
  for I := 0 to cbNeeded div SizeOf(HMODULE) - 1 do
  begin
    if GetModuleFileNameExA(hProc, hMods[I], Name, SizeOf(Name)) > 0 then
    begin
      if Pos(UpperCase(comctl32), UpperCase(string(Name))) > 0 then
      begin
        Ver := GetFileVersionNumber(string(Name));
        if Ver.Major <= 5 then
        begin
          AppValid := False;
        end
        else if Ver.Major >= 6 then
        begin
          OSSupport := True;
        end;
      end;
      Result := True;
    end;
  end;
  if not OSSupport then
    AppValid := False;
end;

// 获得 Dll 的版本信息
function DllGetVersion(const dllname: string;
  var DVI: TDLLVERSIONINFO2): Boolean;
type
  _DllGetVersion = function (var DVI: TDLLVERSIONINFO2): DWORD; stdcall;
var
  hMod:THandle;
  pfDllVersion: _DllGetVersion;
begin
  Result := False;
  hMod := LoadLibrary(PChar(dllname));
  if hMod <> 0 then
  try
    @pfDllVersion := GetProcAddress(hMod, 'DllGetVersion');
    if @pfDllVersion = nil then
      Exit;
    FillChar(DVI, SizeOf(TDLLVERSIONINFO2), 0);
    DVI.info1.cbSize := SizeOf(TDLLVERSIONINFO2);
    Result := pfDllVersion(DVI) and $80000000 = 0;
  finally
    FreeLibrary(hMod);
  end;
end;

// 返回操作系统标识串
function GetOSString: string;
var
  OSPlatform: string;
  BuildNumber: Integer;
begin
  Result := 'Unknown Windows Version';
  OSPlatform := 'Windows';
  BuildNumber := 0;

  case Win32Platform of
    VER_PLATFORM_WIN32_WINDOWS:
      begin
        BuildNumber := Win32BuildNumber and $0000FFFF;
        case Win32MinorVersion of
          0..9:
            begin
              if Trim(Win32CSDVersion) = 'B' then
                OSPlatform := 'Windows 95 OSR2'
              else
                OSPlatform := 'Windows 95';
            end;
          10..89:
            begin
              if Trim(Win32CSDVersion) = 'A' then
                OSPlatform := 'Windows 98'
              else
                OSPlatform := 'Windows 98 SE';
            end;
          90:
            OSPlatform := 'Windows Millennium';
        end;
      end;
    VER_PLATFORM_WIN32_NT:
      begin
        if Win32MajorVersion in [3, 4] then
          OSPlatform := 'Windows NT'
        else if Win32MajorVersion = 5 then
        begin
          case Win32MinorVersion of
            0: OSPlatform := 'Windows 2000';
            1: OSPlatform := 'Windows XP';
          end;
        end
        else if Win32MajorVersion = 7 then
        begin
          OSPlatform := 'Windows 7';
        end
        else if Win32MajorVersion = 8 then
        begin
          if Win32MinorVersion >= 1 then
            OSPlatform := 'Windows 8.1'
          else
            OSPlatform := 'Windows 8';
        end
        else if Win32MajorVersion = 10 then
        begin
          OSPlatform := 'Windows 10';
        end;
        BuildNumber := Win32BuildNumber;
      end;
    VER_PLATFORM_WIN32s:
      begin
        OSPlatform := 'Win32s';
        BuildNumber := Win32BuildNumber;
      end;
  end;
  if (Win32Platform = VER_PLATFORM_WIN32_WINDOWS) or
    (Win32Platform = VER_PLATFORM_WIN32_NT) then
  begin
    if Trim(Win32CSDVersion) = '' then
      Result := Format('%s %d.%d (Build %d)', [OSPlatform, Win32MajorVersion,
        Win32MinorVersion, BuildNumber])
    else
      Result := Format('%s %d.%d (Build %d: %s)', [OSPlatform, Win32MajorVersion,
        Win32MinorVersion, BuildNumber, Win32CSDVersion]);
  end
  else
    Result := Format('%s %d.%d', [OSPlatform, Win32MajorVersion, Win32MinorVersion])
end;

// 得到本机名
function GetComputeNameStr : string;
var
  dwBuff : DWORD;
  aryCmpName : array [0..255] of Char;
begin
  Result := '';
  dwBuff := 256;
  FillChar(aryCmpName, SizeOf(aryCmpName), 0);
  if GetComputerName(aryCmpName, dwBuff) then
    Result := StrPas(aryCmpName);
end;

// 得到本机用户名
function GetLocalUserName: string;
var
  Count: DWORD;
begin
  Count := 256 + 1; // UNLEN + 1
  // set buffer size to 256 + 2 characters
  SetLength(Result, Count);
  if GetUserName(PChar(Result), Count) then
    StrResetLength(Result)
  else
    Result := '';
end;

function REG_CURRENT_VERSION: string;
begin
  if CheckWindows9598 then
    Result := HKLM_CURRENT_VERSION_WINDOWS
  else
    Result := HKLM_CURRENT_VERSION_NT;
end;

function GetRegisteredCompany: string;
begin
  Result := RegReadStringDef(HKEY_LOCAL_MACHINE, REG_CURRENT_VERSION, 'RegisteredOrganization', '');
end;

function GetRegisteredOwner: string;
begin
  Result := RegReadStringDef(HKEY_LOCAL_MACHINE, REG_CURRENT_VERSION, 'RegisteredOwner', '');
end;

//------------------------------------------------------------------------------
// 常用控件辅助过程
//------------------------------------------------------------------------------

procedure ListViewSwapItem(ListView: TListView; Idx1, Idx2: Integer);
var
  S: string;
  Data: Pointer;
  Sel: Boolean;
begin
  S := ListView.Items[Idx1].SubItems.Text;
  Data := ListView.Items[Idx1].Data;
  Sel := ListView.Items[Idx1].Selected;
  ListView.Items[Idx1].SubItems.Assign(ListView.Items[Idx2].SubItems);
  ListView.Items[Idx1].Data := ListView.Items[Idx2].Data;
  ListView.Items[Idx1].Selected := ListView.Items[Idx2].Selected;
  ListView.Items[Idx2].SubItems.Text := S;
  ListView.Items[Idx2].Data := Data;
  ListView.Items[Idx2].Selected := Sel;
end;

procedure ListViewDeleteSelected(ListView: TListView);
var
  I: Integer;
begin
  ListView.Items.BeginUpdate;
  try
    for I := ListView.Items.Count - 1 downto 0 do
      if ListView.Items[I].Selected then
        ListView.Items.Delete(I);
  finally
    ListView.Items.EndUpdate;
  end;
end;

procedure ListViewMoveDownSelected(ListView: TListView);
var
  I: Integer;
begin
  ListView.Items.BeginUpdate;
  try
    for I := ListView.Items.Count - 2 downto 0 do
      if ListView.Items[I].Selected and not ListView.Items[I + 1].Selected then
        ListViewSwapItem(ListView, I, I + 1);
  finally
    ListView.Items.EndUpdate;
  end;
end;

procedure ListViewMoveUpSelected(ListView: TListView);
var
  I: Integer;
begin
  ListView.Items.BeginUpdate;
  try
    for I := 1 to ListView.Items.Count - 1 do
      if ListView.Items[I].Selected and not ListView.Items[I - 1].Selected then
        ListViewSwapItem(ListView, I, I - 1);
  finally
    ListView.Items.EndUpdate;
  end;
end;

// 为 Listbox 增加水平滚动条
procedure ListboxHorizontalScrollbar(Listbox: TCustomListBox);
var
  I: Integer;
  Width, MaxWidth: Integer;
begin
  Assert(Assigned(Listbox));
  MaxWidth := 0;
  for I := 0 to Listbox.Items.Count - 1 do
  begin
    Width := Listbox.Canvas.TextWidth(Listbox.Items[I]) + 4;
    if Width > MaxWidth then
      MaxWidth := Width;
  end;
  if ListBox is TCheckListBox then
    Inc(MaxWidth, GetSystemMetrics(SM_CXMENUCHECK) + 2);
  SendMessage(Listbox.Handle, LB_SETHORIZONTALEXTENT, MaxWidth, 0);
end;

// 复制菜单项和其子项
procedure CloneMenuItem(Source, Dest: TMenuItem);
var
  Item, AItem: TMenuItem;
  I: Integer;
begin
  if (Source <> nil) and (Dest <> nil) then
  begin
    Dest.Clear;

    for I := 0 to Source.Count - 1 do
    begin
      Item := TMenuItem.Create(Dest.Owner);
      AItem := Source.Items[I];

      if AItem.Action <> nil then
        Item.Action := AItem.Action
      else
      begin
        Item.Caption := AItem.Caption;
        Item.Hint := AItem.Hint;
        Item.Tag := AItem.Tag;
        Item.ShortCut := AItem.ShortCut;
        Item.Visible := AItem.Visible;
        Item.OnClick := AItem.OnClick;

        Item.Checked := AItem.Checked;
        Item.ImageIndex := AItem.ImageIndex;
      end;

      Dest.Add(Item);

      CloneMenuItem(Source.Items[I], Item);
    end;
  end;
end;

{$IFDEF MEMO_CARETPOS_BUG}
type
  TCnSelection = record
    StartPos, EndPos: Integer;
  end;
{$ENDIF}

// 封装的获取 Memo 光标位置的方法，修补了 XE3 或以下碰到大文件时出负值的问题
function MemoGetCaretPos(Memo: TCustomMemo): TPoint;
{$IFDEF MEMO_CARETPOS_BUG}
var
  Selection : TCnSelection;
{$ENDIF}
begin
{$IFDEF MEMO_CARETPOS_BUG}
  {$IFDEF SUPPORT_WIN64}
  SendMessage(Memo.Handle, EM_GETSEL, NativeInt(@Selection.StartPos), NativeInt(@Selection.EndPos));
  Result.Y := SendMessage(Memo.Handle, EM_LINEFROMCHAR, WPARAM(Selection.StartPos), 0);
  Result.X := Selection.StartPos - SendMessage(Memo.Handle, EM_LINEINDEX, WPARAM(Result.Y), 0);
  {$ELSE}
  SendMessage(Memo.Handle, EM_GETSEL, LongWord(@Selection.StartPos), LongWord(@Selection.EndPos));
  Result.Y := SendMessage(Memo.Handle, EM_LINEFROMCHAR, WPARAM(Selection.StartPos), 0);
  Result.X := Selection.StartPos - SendMessage(Memo.Handle, EM_LINEINDEX, WPARAM(Result.Y), 0);
  {$ENDIF}
{$ELSE}
  Result := Memo.CaretPos;
{$ENDIF}
end;

{$ENDIF}

procedure SelectMemoOneLine(AMemo: TMemo; FromLine: Integer);
var
  L, I: Integer;
{$IFDEF DELPHI2007}
  J, Len: Integer;
{$ENDIF}
begin
  if AMemo = nil then
    Exit;

  if AMemo.Lines.Count > FromLine then // 有待显示内容
  begin
    L := 0;
    for I := 0 to FromLine - 1 do
    begin
{$IFDEF DELPHI2007}
      Len := Length(AMemo.Lines[I]);
      for J := 0 to Length(AMemo.Lines[I]) - 1 do
      begin
        if Ord(AMemo.Lines[I][J]) < 128 then
          Inc(Len);
      end;
      Len := Len div 2;
      L := L + Len + 2;
{$ELSE}
      L := L + Length(AMemo.Lines[I]) + 2;
{$ENDIF}
    end;

    AMemo.SelStart := L;
    AMemo.SelLength := Length(AMemo.Lines[FromLine]);
  end;
end;

//------------------------------------------------------------------------------
// 其它过程
//------------------------------------------------------------------------------

{$IFDEF MSWINDOWS}

type
  TWinControlAccess = class(TWinControl);
  TControlAccess = class(TControl);

// 获取 Control 表面的位图，ResetSize 为 True 表示使用 Control 尺寸设置位图尺寸
function GetControlBitmap(AControl: TControl; Bmp: TBitmap;
  ResetSize: Boolean): Boolean;
begin
  Result := False;
  if (AControl = nil) or (Bmp = nil) then
    Exit;

  if ResetSize then
  begin
    Bmp.PixelFormat := pf24Bit;
    Bmp.Canvas.Brush.Color := TControlAccess(AControl).Color;
    Bmp.Width := AControl.Width;
    Bmp.Height := AControl.Height;
  end;

  if AControl is TWinControl then
    TWinControlAccess(AControl).PaintWindow(Bmp.Canvas.Handle)
  else
    AControl.Perform(WM_PAINT, Bmp.Canvas.Handle, 0);
  Result := True;
end;

// 获得多显示器情况下，整个桌面相对于主显示器原点的坐标
function GetMultiMonitorDesktopRect: TRect;
var
  I: Integer;
begin
  Result.Left := 0;
  Result.Top := 0;
  Result.Bottom := Screen.DesktopHeight;
  Result.Right := Screen.DesktopWidth;

  for I := 0 to Screen.MonitorCount - 1 do
  begin
    if Screen.Monitors[I].Left < Result.Left then
      Result.Left := Screen.Monitors[I].Left;
    if Screen.Monitors[I].Top < Result.Top then
      Result.Top := Screen.Monitors[I].Top;
    if Screen.Monitors[I].Height + Screen.Monitors[I].Top > Result.Bottom then
      Result.Bottom := Screen.Monitors[I].Height + Screen.Monitors[I].Top;
    if Screen.Monitors[I].Width + Screen.Monitors[I].Left > Result.Right then
      Result.Right := Screen.Monitors[I].Width + Screen.Monitors[I].Left;
  end;
end;

{$ENDIF}

// 输出限制在 Min..Max 之间
function TrimInt(Value, Min, Max: Integer): Integer;
begin
  if Value > Max then
    Result := Max
  else if Value < Min then
    Result := Min
  else
    Result := Value;
end;

// 比较两个整数，V1 > V2 返回 1，V1 < V2 返回 -1，V1 = V2 返回 0
// 如果 Desc 为 True，返回结果反向
function CompareInt(V1, V2: Integer; Desc: Boolean = False): Integer;
begin
  if V1 > V2 then
    Result := 1
  else if V1 < V2 then
    Result := -1
  else // V1 = V2
    Result := 0;
  if Desc then
    Result := -Result;
end;

// 输出限制在 0..255 之间
function IntToByte(Value: Integer): Byte;
{$IFDEF MSWINDOWS}
asm
        OR     EAX, EAX
        JNS    @@Positive
        XOR    EAX, EAX
        RET

@@Positive:
        CMP    EAX, 255
        JBE    @@OK
        MOV    EAX, 255
@@OK:
{$ELSE}
begin
  Result := Byte(Value);
{$ENDIF}
end;

// 由 TRect 分离出坐标、宽高
procedure DeRect(Rect: TRect; var x, y, Width, Height: Integer);
begin
  x := Rect.Left;
  y := Rect.Top;
  Width := Rect.Right - Rect.Left;
  Height := Rect.Bottom - Rect.Top;
end;

// 比较两个 Rect
function RectEqu(Rect1, Rect2: TRect): Boolean;
begin
  Result := (Rect1.Left = Rect2.Left) and (Rect1.Top = Rect2.Top) and
    (Rect1.Right = Rect2.Right) and (Rect1.Bottom = Rect2.Bottom);
end;

// 产生 TSize 类型
function EnSize(cx, cy: Integer): TSize;
begin
  Result.cx := cx;
  Result.cy := cy;
end;

// 计算 Rect 的宽度
function RectWidth(Rect: TRect): Integer;
begin
  Result := Rect.Right - Rect.Left;
end;

// 计算 Rect 的高度
function RectHeight(Rect: TRect): Integer;
begin
  Result := Rect.Bottom - Rect.Top;
end;

// 判断范围
function InBound(Value: Integer; V1, V2: Integer): Boolean;
begin
  Result := (Value >= Min(V1, V2)) and (Value <= Max(V1, V2));
end;

// 比较两个方法地址是否相等
function SameMethod(Method1, Method2: TMethod): Boolean;
begin
  Result := CompareMem(@Method1, @Method2, SizeOf(TMethod));
end;

// 二分法在列表中查找
function HalfFind(List: TList; P: Pointer; SCompare: TListSortCompare): Integer;
var
  L, R, M: Integer;
  Res: Integer;
begin
  Result := -1;
  L := 0;
  R := List.Count - 1;
  if R < L then Exit;
  if SCompare(P, List[L]) < 0 then Exit;
  if SCompare(P, List[R]) > 0 then Exit;
  while True do
  begin
    M := (L + R) shr 1;
    Res := SCompare(P, List[M]);
    if Res > 0 then
      L := M
    else if Res < 0 then
      R := M
    else
    begin
      Result := M;
      Exit;
    end;
    if L = R then
      Exit
    else if R - L = 1 then
    begin
      if SCompare(P, List[L]) = 0 then
        Result := L
      else if SCompare(P, List[R]) = 0 then
        Result := R;
      Exit;
    end;
  end;
end;

// 二分法在排序列表中查找，支持重复记录，返回一个范围值
function HalfFindEx(List: TList; P: Pointer; SCompare: TListSortCompare): TFindRange;
var
  I, Idx: Integer;
begin
  Idx := HalfFind(List, P, SCompare);
  Result.tgFirst := Idx;
  for I := Idx - 1 downto 0 do
    if SCompare(P, List[I]) = 0 then
      Result.tgFirst := I
    else
      Break;
  Result.tgLast := Idx;
  for I := Idx + 1 to List.Count - 1 do
    if SCompare(P, List[I]) = 0 then
      Result.tgLast := I
    else
      Break;
end;

{$WARNINGS OFF}

// 检查中国大陆的 18 位身份证是否合法
function CheckChineseIDCardNumber(const IDNumber: string): Boolean;
const
  IDFactors: array[1..17] of Integer = (7, 9, 10, 5, 8, 4, 2, 1, 6, 3, 7, 9, 10, 5, 8, 4, 2);
  Remains: array[0..10] of Char = ('1', '0', 'X', '9', '8', '7', '6', '5', '4', '3', '2');
var
  I, Sum: Integer;
begin
  Result := False;
  if Length(IDNumber) <> 18 then
    Exit;

  Sum := 0;
  for I := 1 to 17 do
  begin
    if not (IDNumber[I] in ['0'..'9']) then
      Exit;

    Sum := Sum + (Ord(IDNumber[I]) - 48) * IDFactors[I];
  end;
  Sum := Sum mod 11;
  if Remains[Sum] = UpperCase(IDNumber[18]) then
    Result := True;
end;

{$WARNINGS ON}

{$IFDEF MSWINDOWS}

// 拉伸绘制 ImageList 中的指定图像至指定 Canvas 的指定矩形中
procedure StretchDrawImageListToCanvas(ImageList: TImageList; ImageIndex: Integer;
  DestCanvas: TCanvas; X, Y, AWidth, AHeight: Integer);
var
  Icon: TIcon;
begin
  if (ImageList = nil) or (ImageIndex >= ImageList.Count) then
    Exit;

  Icon := TIcon.Create;
  try
    ImageList.GetIcon(ImageIndex, Icon);
    DrawIconEx(DestCanvas.Handle, X, Y, Icon.Handle, AWidth, AHeight, 0, 0, DI_NORMAL);
  finally
    Icon.Free;
  end;
end;

{$ENDIF}

// 交换两个数
procedure CnSwap(var A, B: Byte); overload;
var
  Tmp: Byte;
begin
  Tmp := A;
  A := B;
  B := Tmp;
end;

procedure CnSwap(var A, B: Integer); overload;
var
  Tmp: Integer;
begin
  Tmp := A;
  A := B;
  B := Tmp;
end;

procedure CnSwap(var A, B: Single); overload;
var
  Tmp: Single;
begin
  Tmp := A;
  A := B;
  B := Tmp;
end;

procedure CnSwap(var A, B: Double); overload;
var
  Tmp: Double;
begin
  Tmp := A;
  A := B;
  B := Tmp;
end;

{$IFDEF MSWINDOWS}

// 延时
procedure Delay(const uDelay: DWORD);
var
  N: DWORD;
begin
  N := GetTickCount;
  while GetTickCount - N <= uDelay do
    Application.ProcessMessages;
end;



// 把指定内存内容以指定格式设置入剪贴板
procedure SetClipboardContent(Format: Word; var Buffer; Size: Integer);
var
  Data: THandle;
  DataPtr: Pointer;
begin
  OpenClipboard(0);
  try
    Data := GlobalAlloc(GMEM_MOVEABLE + GMEM_DDESHARE, Size);
    try
      DataPtr := GlobalLock(Data);
      try
        Move(Buffer, DataPtr^, Size);
        EmptyClipboard;
        SetClipboardData(Format, Data);
      finally
        GlobalUnlock(Data);
      end;
    except
      GlobalFree(Data);
      raise;
    end;
  finally
    CloseClipboard;
  end;
end;

{$IFNDEF WIN64}

// 在 Win9X 下让喇叭发声
procedure BeepEx(const Freq: WORD = 1200; const Delay: WORD = 1);
const
  FREQ_SCALE = $1193180;
var
  Temp: WORD;
begin
  Temp := FREQ_SCALE div Freq;
  asm
    in al,61h;
    or al,3;
    out 61h,al;
    mov al,$b6;
    out 43h,al;
    mov ax,temp;
    out 42h,al;
    mov al,ah;
    out 42h,al;
  end;
  Sleep(Delay);
  asm
    in al,$61;
    and al,$fc;
    out $61,al;
  end;
end;

{$ENDIF}

function GetLastErrorMsg(IncludeErrorCode: Boolean): string;
var
  ErrNo: Integer;
  Buf: array[0..255] of Char;
begin
  ErrNo := GetLastError;
  FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM, nil, ErrNo, $400, Buf, 255, nil);
  if Buf = '' then StrCopy(@Buf, PChar(SUnknowError));
  Result := Buf;
  if IncludeErrorCode then
    Result := Result + #10#13 + SErrorCode + IntToStr(ErrNo);
end;

// 显示 Win32 Api 运行结果信息
procedure ShowLastError;
begin
  MessageBox(Application.Handle, PChar(GetLastErrorMsg),
    PChar(SCnInformation), MB_OK + MB_ICONINFORMATION);
end;

{$ENDIF}

{$IFDEF UNICODE}

// 取汉字的拼音，参数为 Utf16
function GetHzPyW(const AHzStr: string): string;
begin
  Result := string(GetHzPy(AnsiString(AHzStr)));
end;

{$ENDIF}

// 取汉字的拼音首字母
function GetHzPy(const AHzStr: AnsiString): AnsiString;
const
  ChinaCode: array[0..25, 0..1] of Integer = ((1601, 1636), (1637, 1832), (1833, 2077),
    (2078, 2273), (2274, 2301), (2302, 2432), (2433, 2593), (2594, 2786), (9999, 0000),
    (2787, 3105), (3106, 3211), (3212, 3471), (3472, 3634), (3635, 3722), (3723, 3729),
    (3730, 3857), (3858, 4026), (4027, 4085), (4086, 4389), (4390, 4557), (9999, 0000),
    (9999, 0000), (4558, 4683), (4684, 4924), (4925, 5248), (5249, 5589)); // 仨 9999 0000 表示没有 i u v 开头的拼音
var
  I, J, HzOrd: Integer;
begin
  Result := '';
  I := 1;
  while I <= Length(AHzStr) do
  begin
    if (AHzStr[I] >= #160) and (AHzStr[I + 1] >= #160) then
    begin
      HzOrd := (Ord(AHzStr[I]) - 160) * 100 + Ord(AHzStr[I + 1]) - 160;
      for J := 0 to 25 do
      begin
        if (HzOrd >= ChinaCode[J][0]) and (HzOrd <= ChinaCode[J][1]) then
        begin
          Result := Result + AnsiChar(Byte('A') + J);
          Break;
        end;
      end;
      Inc(I);
    end
    else
      Result := Result + AHzStr[I];
    Inc(I);
  end;
end;

{$IFDEF MSWINDOWS}

// 全角字符转换为半角字符。其中句号"。"转为"."，顿号"、"转为","
function TextFullWidthToHalfWidth(const Text: string): string;
var
  S: string;
  S1, S2: WideString;
  l: Integer;
begin
  // 中文句号和顿号不会自动替换为 . 号，需要自行处理
  S := StringReplace(Text, '。', '.', [rfReplaceAll]);
  S := StringReplace(S, '、', ',', [rfReplaceAll]);
  S1 := S;
  l := Length(S1);
  SetLength(S2, l);
  LCMapStringW(GetThreadLocale, LCMAP_HALFWIDTH, PWideChar(S1), l, PWideChar(S2), l);
  Result := S2;
end;

// 半角字符转换为全角字符
function TextHalfWidthToFullWidth(const Text: string): string;
var
  S1, S2: WideString;
  l: Integer;
begin
  S1 := Text;
  l := Length(S1);
  SetLength(S2, l);
  LCMapStringW(GetThreadLocale, LCMAP_FULLWIDTH, PWideChar(S1), l, PWideChar(S2), l);
  Result := S2;
end;

// 获得 CustomEdit 选中的字符串，可以处理 XP 以上的系统
function GetSelText(Edt: TCustomEdit): string;
var
  Ver: TDLLVERSIONINFO2;
  iSelStart, Len: Integer;
  I, J, itemp: Integer;
  stext: string;
begin
  Assert(Assigned(Edt));
  Result := Edt.SelText;
  if not DllGetVersion('comctl32.dll', Ver) then
    Exit;
  if Ver.info1.dwMajorVersion <= 5 then
    Exit;
  with Edt do
  begin
    Result := '';
    if SelLength <= 0 then
      Exit;

    stext := Edt.Text;
    iSelStart := 0;
    I := 0;
    J := 1;
    itemp := SelStart;
    while I < itemp do
    begin
      if ByteType(stext, J) <> mbLeadByte then
        Inc(I);
      Inc(iSelStart);
      Inc(J);
    end;
    Len := SelLength;
    I := 0;
    J := 1;
    while I < Len do
    begin
      Result := Result + stext[iSelStart + J];
      if ByteType(stext, iSelStart + J) <> mbLeadByte then
        Inc(I);
      Inc(J);
    end;
  end;
end;

{$ENDIF}

// 删除空行和每一行的行首尾空格
procedure TrimStrings(AList: TStrings);
var
  I: Integer;
begin
  for I := AList.Count - 1 downto 0 do
  begin
    AList[I] := Trim(AList[I]);
    if AList[I] = '' then
      AList.Delete(I);
  end;
end;

// 字符串列表去重
procedure RemoveDuplicatedStrings(AList: TStrings; CaseSensitive: Boolean);
var
  I, J: Integer;
  V: string;
  Dup: Boolean;
begin
  if (AList = nil) or (AList.Count <= 1) then
    Exit;

  if CaseSensitive then
  begin
    for I := AList.Count - 1 downto 0 do
    begin
      V := AList[I];
      Dup := False;

      for J := 0 to I - 1 do
      begin
        if V = AList[J] then
        begin
          Dup := True;
          Break;
        end;
      end;

      if Dup then
        AList.Delete(I);
    end;
  end
  else
  begin
    for I := AList.Count - 1 downto 0 do
    begin
      V := UpperCase(AList[I]);
      Dup := False;

      for J := 0 to I - 1 do
      begin
        if V = UpperCase(AList[J]) then
        begin
          Dup := True;
          Break;
        end;
      end;

      if Dup then
        AList.Delete(I);
    end;
  end;
end;

procedure RemoveDuplicatedStrings(AList: TCnWideStringList; CaseSensitive: Boolean);
var
  I, J: Integer;
  V: string;
  Dup: Boolean;
begin
  if (AList = nil) or (AList.Count <= 1) then
    Exit;

  if CaseSensitive then
  begin
    for I := AList.Count - 1 downto 0 do
    begin
      V := AList[I];
      Dup := False;

      for J := 0 to I - 1 do
      begin
        if V = AList[J] then
        begin
          Dup := True;
          Break;
        end;
      end;

      if Dup then
        AList.Delete(I);
    end;
  end
  else
  begin
    for I := AList.Count - 1 downto 0 do
    begin
      V := UpperCase(AList[I]);
      Dup := False;

      for J := 0 to I - 1 do
      begin
        if V = UpperCase(AList[J]) then
        begin
          Dup := True;
          Break;
        end;
      end;

      if Dup then
        AList.Delete(I);
    end;
  end;
end;

{$IFDEF MSWINDOWS}

// 声卡是否存在
function SoundCardExist: Boolean;
begin
  Result := WaveOutGetNumDevs > 0;
end;

{$ENDIF}

// 判断 ASrc 是否派生自类名为 AClass 的类
function InheritsFromClassName(ASrc: TClass; const AClass: string): Boolean;
begin
  Result := False;
  while ASrc <> nil do
  begin
    if ASrc.ClassNameIs(AClass) then
    begin
      Result := True;
      Exit;
    end;
    ASrc := ASrc.ClassParent;
  end;
end;

// 判断 AObject 是否派生自类名为 AClass 的类
function InheritsFromClassName(AObject: TObject; const AClass: string): Boolean;
begin
  Result := InheritsFromClassName(AObject.ClassType, AClass);
end;

{$IFDEF MSWINDOWS}

// 提升自身权限到 SeDebug 或取消此权限
function AdjustDebugPrivilege(Enable: Boolean): Boolean;
var
  Token: THandle;

  function InternalEnablePrivilege(Token: Cardinal; PrivName: string; Enable: Boolean): Boolean;
  var
    TP {$IFDEF FPC}, Prev {$ENDIF}: TOKEN_PRIVILEGES;
    Dummy: Cardinal;
  begin
    TP.PrivilegeCount := 1;
    LookupPrivilegeValue(nil, PChar(PrivName), TP.Privileges[0].Luid);
    if Enable then
      TP.Privileges[0].Attributes := SE_PRIVILEGE_ENABLED
    else
      TP.Privileges[0].Attributes := 0;

{$IFDEF FPC}
    AdjustTokenPrivileges(Token, False, TP, SizeOf(TP), Prev, Dummy);
{$ELSE}
    AdjustTokenPrivileges(Token, False, TP, SizeOf(TP), nil, Dummy);
{$ENDIF}
    Result := GetLastError = ERROR_SUCCESS;
  end;

begin
  OpenProcessToken(GetCurrentProcess, TOKEN_ADJUST_PRIVILEGES, Token);
  Result := InternalEnablePrivilege(Token, 'SeDebugPrivilege', Enable);
  CloseHandle(Token);
end;

{$IFNDEF WIN64}

// 查找 PEB 结构内容以判定本进程是否被调试中
function PEBIsDebugged: Boolean;
asm
        MOV     EAX, FS:[$30];
        MOV     AL, BYTE PTR DS:[EAX + 02];
end;

{$ENDIF}

// 使用 NtQueryInformationProcess 以判定本进程是否被调试中
function NtIsDeugged: Boolean;
var
  DebugPort: DWORD;
begin
  Result := False;
  if not Assigned(NtQueryInformationProcess) then
  begin
    if (Win32Platform = VER_PLATFORM_WIN32_NT)
      and (Win32MajorVersion >= 5) and (Win32MinorVersion >= 1) then
    begin
      if NtDllHandle = 0 then
      begin
        NtDllHandle := LoadLibrary('NTDLL.DLL');
        if NtDllHandle = 0 then
          raise Exception.Create('Can NOT Load NTDLL.');
      end;

      NtQueryInformationProcess := GetProcAddress(NtDllHandle, 'NtQueryInformationProcess');
    end
    else
      raise Exception.Create('Current Windows NOT Support.');

    if not Assigned(NtQueryInformationProcess) then
      raise Exception.Create('NtQueryInformationProcess NOT Found in NTDLL.');

    if 0 <> NtQueryInformationProcess(GetCurrentProcess, 7, @DebugPort, SizeOf(DebugPort), nil) then
      raise Exception.Create('Call NtQueryInformationProcess Failed.');

    Result := DebugPort <> 0;
  end;
end;

// 根据文件名结束进程，不区分路径
procedure KillProcessByFileName(const FileName: String);
var
  ID: DWORD;
  S, Tmp: string;
  Ret: Boolean;
  SnapshotHandle: THandle;
  PE32: TProcessEntry32;
  H: HWND;
begin
  S := LowerCase(FileName);
  SnapshotHandle := CreateToolHelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  PE32.dwSize := SizeOf(PE32);
  Ret := Process32First(SnapshotHandle, PE32);
  while Integer(Ret) <> 0 do
  begin
    Tmp := LowerCase(PE32.szExeFile);
    if Pos(S, Tmp) > 0 then
    begin
      Id := PE32.th32ProcessID;
      H := OpenProcess(PROCESS_ALL_ACCESS, True, Id);
      TerminateProcess(H, 0);
    end;
    Ret := Process32Next(SnapshotHandle,PE32);
  end;
end;

// 根据完整文件名结束进程，区分路径，32/64 交叉 Kill 32/64 通过
function KillProcessByFullFileName(const FullFileName: string): Boolean;
type
  TGetProcessImageFileNameProc = function (hProcess: THandle; ImageFileName: PChar; nBufferLength: DWORD): BOOL; stdcall;
var
  SnapshotHandle, PH, HPsApi: THandle;
  ProcessEntry: TProcessEntry32;
  TargetProcessPath, TargetDosPath: string;
  FullDevName, DriverName, DevName: string;
  Proc: TGetProcessImageFileNameProc;
begin
  Result := False;
  SnapshotHandle := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  if SnapshotHandle = INVALID_HANDLE_VALUE then
    Exit;

  ProcessEntry.dwSize := SizeOf(TProcessEntry32);
  if not Process32First(SnapshotHandle, ProcessEntry) then
  begin
    CloseHandle(SnapshotHandle);
    Exit;
  end;

  Result := False;
  Proc := nil;
  HPsApi := 0;
  FullDevName := '';

  try
    repeat
      PH := OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ or PROCESS_TERMINATE,
        False, ProcessEntry.th32ProcessID);
      if PH <> 0 then
      begin
        // 获取目标进程的完整路径
        SetLength(TargetProcessPath, MAX_PATH);
        SetLength(TargetDosPath, MAX_PATH);

        // 注意如果对方是 64 位，则要换 GetProcesslmageFileName
        if CheckProcess64(PH) then
        begin
          if not Assigned(Proc) then
          begin
            HPsApi := LoadLibrary('PSAPI.dll');
            if HPsApi > 32 then
            begin
{$IFDEF UNICODE}
              Proc := TGetProcessImageFileNameProc(GetProcAddress(HPsApi, 'GetProcessImageFileNameW'));
{$ELSE}
              Proc := TGetProcessImageFileNameProc(GetProcAddress(HPsApi, 'GetProcessImageFileNameA'));
{$ENDIF}
            end;
          end;

          if Assigned(Proc) then
          begin
            if Proc(PH, PChar(TargetProcessPath), Length(TargetProcessPath)) then
            begin
              if FullDevName = '' then // 把原始逻辑盘的 C:\a.exe 这种转换为 \Device\Harddisk1\a.exe 这种
              begin
                if (Length(FullFileName) > 3) and (FullFileName[2] = ':') and (FullFileName[3] = '\') then
                begin
                  DriverName := Copy(FullFileName, 1, 2);
                  TargetDosPath := Copy(FullFileName, 3, MaxInt);
                  SetLength(DevName, MAX_PATH);
                  if QueryDosDevice(PChar(DriverName), PChar(DevName), Length(DevName)) > 0 then
                  begin
                    SetLength(DevName, StrLen(PChar(DevName)));
                    FullDevName := DevName + TargetDosPath;
                  end;
                end;
              end;

              // 此处的完整路径是 \Device\ 开头，不能和 FullFileName 直接比较
              if StrComp(PChar(TargetProcessPath), PChar(FullDevName)) = 0 then
                Result := TerminateProcess(PH, 0);
            end;
          end;
          CloseHandle(PH); // 无论是否获取成功都关闭 Handle
        end
        else
        begin
          if GetModuleFileNameEx(PH, 0, PChar(TargetProcessPath), Length(TargetProcessPath)) > 0 then
          begin
            // 比较完整路径，相等则尝试结束进程
            if StrComp(PChar(TargetProcessPath), PChar(FullFileName)) = 0 then
              Result := TerminateProcess(PH, 0);
          end;

          CloseHandle(PH); // 无论是否获取成功都关闭 Handle
        end;
      end;
    until not Process32Next(SnapshotHandle, ProcessEntry);
  finally
    if HPsApi > 32 then
      FreeLibrary(HPsApi);
  end;
  CloseHandle(SnapshotHandle);
end;

{$ENDIF}

// 获得级联属性信息
function GetPropInfoIncludeSub(Instance: TObject; const PropName: string;
  AKinds: TTypeKinds): PPropInfo;
var
  AObject: TObject;
  Dot: Integer;
  RestProp: String;
begin
  Dot := Pos('.', PropName);
  if Dot = 0 then
  begin
    Result := GetPropInfo(Instance, PropName, AKinds);
  end
  else
  begin
    if GetPropInfo(Instance, Copy(PropName, 1, Dot - 1)) <> nil then
    begin
      AObject := GetObjectProp(Instance, Copy(PropName, 1, Dot - 1));
      if AObject = nil then
        Result :=  nil
      else
      begin
        RestProp := Copy(PropName, Dot + 1, Length(PropName) - Dot);
        Result := GetPropInfoIncludeSub(AObject, RestProp, AKinds);
      end;
    end
    else
      Result := nil;
  end;
end;

// 获得级联属性值
function GetPropValueIncludeSub(Instance: TObject; PropName: string;
  PreferStrings: Boolean = True): Variant;
const
  SCnControlFont = '!Font';
var
  AObject: TObject;
  Dot: Integer;
  RestProp: String;
  IntToId: TIntToIdent;
  IdValue: String;
  PropInfo: PPropInfo;
begin
  Result := Null;
  if Instance = nil then
    Exit;

  Dot := Pos('.', PropName);
  if Dot = 0 then
  begin
    if (Instance is TStrings) and (PropName = 'Text') then
    begin
      Result := (Instance as TStrings).Text;
      Exit;
    end
    else if Instance.ClassNameIs('TListItem') and (PropName = 'Caption') then
    begin
      Result := GetStrProp(Instance, PropName);
      // Result := (Instance as TListItem).Caption;
      Exit;
    end
    else if Instance.ClassNameIs('TTreeNode') and (PropName = 'Text') then
    begin
      Result := GetStrProp(Instance, PropName);
      // Result := (Instance as TTreeNode).Text;
      Exit;
    end
    else if PropName = SCnControlFont then // 在此内部处理 !Font 的情况
    begin
      PropName := 'Font';
      PropInfo := GetPropInfo(Instance, PropName);
      if PropInfo = nil then
        Exit;

      if PropInfo^.PropType^.Kind = tkClass then
      begin
        try
          Result := FontToString(TFont(GetObjectProp(Instance, PropName)));
        except
          ;
        end;
        Exit;
      end;
    end;

    PropInfo := GetPropInfo(Instance, PropName);
    if PropInfo = nil then
      Exit;

    if PropInfo^.PropType^.Kind = tkClass then
    begin
      AObject := GetObjectProp(Instance, PropName);
      // PreferStrings 时优先返回组件名
      if PreferStrings and (AObject <> nil) and (AObject is TComponent) then
        Result := (AObject as TComponent).Name
      else
        Result := TCnNativeInt(AObject);
      Exit;
    end;

    Result := GetPropValue(Instance, PropName, PreferStrings);
    if (Result <> Null) and IsInt(Result) then   // 如果返回整数，尝试将其转换成常量。
    begin
      if PropInfo^.PropType^.Kind = tkInteger then
      begin
{$IFDEF FPC}
        IntToId := FindIntToIdent(PPropInfo(PropInfo)^.PropType);
{$ELSE}
        IntToId := FindIntToIdent(PPropInfo(PropInfo)^.PropType^);
{$ENDIF}
        if Assigned(IntToId) and IntToId(Result, IdValue) then
          Result := IdValue;
      end
    end
  end
  else
  begin
    // 递归寻找
    AObject := nil;
    if GetPropInfo(Instance, Copy(PropName, 1, Dot - 1)) <> nil then
      AObject := GetObjectProp(Instance, Copy(PropName, 1, Dot - 1));

    if AObject = nil then
      Result :=  Null
    else
    begin
      RestProp := Copy(PropName, Dot + 1, Length(PropName) - Dot);
      Result := GetPropValueIncludeSub(AObject, RestProp, PreferStrings);
    end;
  end;
end;

// 设置级联属性值，不处理异常
procedure DoSetPropValueIncludeSub(Instance: TObject; const PropName: string;
  Value: Variant; AOwner: TComponent = nil);
var
  AObject: TObject;
  Dot, IntValue: Integer;
  RestProp: string;
  PropInfo: PPropInfo;
  IdToInt: TIdentToInt;
  S, AName: string;
  AComp: TComponent;
begin
  Dot := Pos('.', PropName);
  if Dot = 0 then
  begin
    PropInfo := GetPropInfo(Instance, PropName);
    if PropInfo^.PropType^.Kind = tkInteger then
    begin
{$IFDEF FPC}
      IdToInt := FindIdentToInt(PPropInfo(PropInfo)^.PropType);
{$ELSE}
      IdToInt := FindIdentToInt(PPropInfo(PropInfo)^.PropType^);
{$ENDIF}
      if Assigned(IdToInt) and IdToInt(Value, IntValue) then
        SetPropValue(Instance, PropName, IntValue)
      else
      begin
{$IFDEF FPC}
        S := VarToStrDef(Value, '');
{$ELSE}
        S := VarToStr(Value);
{$ENDIF}
        if (S <> '') and (Length(S) > 1) and (S[1] = '$') then // 是十六进制
        begin
          if IsInt(S) then
          begin
            SetPropValue(Instance, PropName, StrToInt(S));
            Exit;
          end;
        end;
        SetPropValue(Instance, PropName, Value)
      end;
    end
    else
    begin
      if (PropInfo^.PropType^.Kind in [tkSet, tkEnumeration]) and
        (VarType(Value) <> varInteger) then
      begin
        Value := Trim(Value);
        SetPropValue(Instance, PropName, Value);
      end
      else if PropInfo^.PropType^.Kind = tkClass then
      begin
        AName := VarToStr(Value);
        AComp := nil;
        if (AOwner <> nil) and (AName <> '') then
          AComp := AOwner.FindComponent(AName);
        SetObjectProp(Instance, PropName, AComp);
      end
      else
        SetPropValue(Instance, PropName, Value);
    end
  end
  else
  begin
    // 递归设置
    AObject := GetObjectProp(Instance, Copy(PropName, 1, Dot - 1));
    RestProp := Copy(PropName, Dot + 1, Length(PropName) - Dot);
    DoSetPropValueIncludeSub(AObject, RestProp, Value);
  end;
end;

// 设置级联属性值
function SetPropValueIncludeSub(Instance: TObject; const PropName: string;
  const Value: Variant; AOwner: TComponent = nil): Boolean;
begin
  try
    DoSetPropValueIncludeSub(Instance, PropName, Value, AOwner);
    Result := True;
  except
    Result := False;
  end;
end;

// 获取某属性所在的定义的父类，无则返回 nil
function GetOriginalClassFromProperty(AClass: TClass; const PropName: string): TClass;
var
  PropInfo: PPropInfo;
  CT: TClass;
begin
  Result := nil;
  PropInfo := GetPropInfo(AClass, PropName);
  if PropInfo = nil then
    Exit;

  CT := AClass;
  while CT <> nil do
  begin
    if GetPropInfo(CT, PropName) <> nil then
      Result := CT
    else // 父类无此属性，说明在上一个循环的 CT 中
      Break;

    CT := CT.ClassParent;
  end;
end;

// 获取某属性所在的定义的父类，无则返回 nil
function GetOriginalClassFromProperty(AObject: TObject; const PropName: string): TClass;
begin
  if AObject <> nil then
    Result := GetOriginalClassFromProperty(AObject.ClassType, PropName)
  else
    Result := nil;
end;

// 字符串转集合值
function StrToSetValue(const Value: string; PInfo: PTypeInfo): Integer;
var
  EnumInfo: PTypeInfo;
  EnumValue: 0..SizeOf(Integer) * 8 - 1;
  S: string;
  Strings: TStrings;
  I: Integer;
begin
  Result := 0;
  S := Trim(Value);
  if S = '' then Exit;
  if S[1] = '[' then
    Delete(S, 1, 1);
  if S = '' then Exit;
  if S[Length(S)] = ']' then
    Delete(S, Length(S), 1);

{$IFDEF FPC}
  EnumInfo := GetTypeData(PInfo).CompType;
{$ELSE}
  EnumInfo := GetTypeData(PInfo).CompType^;
{$ENDIF}

  Strings := TStringList.Create;
  try
    Strings.CommaText := S;
    for I := 0 to Strings.Count - 1 do
    begin
      EnumValue := GetEnumValue(EnumInfo, Trim(Strings[I]));
      if (EnumValue < GetTypeData(EnumInfo)^.MinValue) or
        (EnumValue > GetTypeData(EnumInfo)^.MaxValue) then
        Exit;                       // 不是有效的枚举值
      Include(TIntegerSet(Result), EnumValue);
    end;
  finally
    Strings.Free;
  end;
end;

function PropInfoName(PropInfo: PPropInfo): string;
begin
  Result := string(PropInfo^.Name);
end;

function TypeInfoName(TypeInfo: PTypeInfo): string;
begin
  Result := string(TypeInfo^.Name);
end;

// 获得某对象的所有属性的字符串值，包括子属性的属性
// IncludeType 为 True 时，格式为 Name=TypeName，Object 中放入 PropType
procedure GetAllPropNames(AComp: TObject; PropNames: TStrings;
  const BaseName: string; IncludeType: Boolean);
var
  I, APropCount: Integer;
  PropListPtr: PPropList;
  PropInfo: PPropInfo;
  TpInfo: PTypeInfo;
  AObj: TObject;
begin
  if PropNames = nil then
    Exit;

  APropCount := GetTypeData(PTypeInfo(AComp.ClassInfo))^.PropCount;
  if APropCount > 0 then
  begin
    GetMem(PropListPtr, APropCount * SizeOf(Pointer));
    GetPropList(PTypeInfo(AComp.ClassInfo), tkAny, PropListPtr);

    try
      for I := 0 to APropCount - 1 do
      begin
        PropInfo := PropListPtr^[I];
        if PropInfo^.Name = '' then
          Continue;

{$IFDEF FPC}
        TpInfo := PropInfo^.PropType;
{$ELSE}
        TpInfo := PropInfo^.PropType^;
{$ENDIF}
        if TpInfo^.Kind in (tkProperties + tkMethods) then
        begin
          if BaseName = '' then
          begin
            if not IncludeType then
              PropNames.Add(PropInfoName(PropInfo))
            else
              PropNames.AddObject(PropInfoName(PropInfo) + '=' +
                TypeInfoName(TpInfo), TObject(TpInfo^.Kind))
          end
          else
          begin
            if not IncludeType then
              PropNames.Add(BaseName + '.' + PropInfoName(PropInfo))
            else
              PropNames.AddObject(BaseName + '.' + PropInfoName(PropInfo) + '=' +
                TypeInfoName(TpInfo), TObject(TpInfo^.Kind))
          end;

          if TpInfo^.Kind = tkClass then
          begin
            AObj := GetObjectProp(AComp, PropInfo);
            if (AObj <> nil) then // 暂只处理非组件的属性、和属于自己的组件属性
              if not (AObj is TComponent) or ((AObj as TComponent).Owner = AComp) then
                GetAllPropNames(AObj, PropNames, PropInfoName(PropInfo), IncludeType);
          end;
        end;
      end;
    finally
      FreeMem(PropListPtr);
    end;
  end;
end;

{* 获得某类的所有属性的字符串值，不包括子属性的属性，因为没有实例
   IncludeType 为 True 时，格式为 Name=TypeName，Object 中放入 PropType }
procedure GetAllPropNamesFromClass(ACompClass: TClass; PropNames: TStrings;
  IncludeType: Boolean);
var
  I, APropCount: Integer;
  PropListPtr: PPropList;
  PropInfo: PPropInfo;
  TpInfo: PTypeInfo;
begin
  if PropNames = nil then
    Exit;

  APropCount := GetTypeData(PTypeInfo(ACompClass.ClassInfo))^.PropCount;
  if APropCount > 0 then
  begin
    GetMem(PropListPtr, APropCount * SizeOf(Pointer));
    GetPropList(PTypeInfo(ACompClass.ClassInfo), tkAny, PropListPtr);

    try
      for I := 0 to APropCount - 1 do
      begin
        PropInfo := PropListPtr^[I];
        if PropInfo^.Name = '' then
          Continue;

{$IFDEF FPC}
        TpInfo := PropInfo^.PropType;
{$ELSE}
        TpInfo := PropInfo^.PropType^;
{$ENDIF}
        if TpInfo^.Kind in (tkProperties + tkMethods) then
        begin
          if not IncludeType then
            PropNames.Add(PropInfoName(PropInfo))
          else
            PropNames.AddObject(PropInfoName(PropInfo) + '=' +
              TypeInfoName(TpInfo), TObject(TpInfo^.Kind))
        end;
      end;
    finally
      FreeMem(PropListPtr);
    end;
  end;
end;

// 封装的获取系统启动以来的毫秒数的函数
function CnGetTickCount: Cardinal;
begin
{$IFDEF MSWINDOWS}
  Result := GetTickCount;
{$ELSE}
  Result := TThread.GetTickCount;
{$ENDIF}
end;

{$IFDEF MSWINDOWS}

// 判断某 Control 的 ParentFont 属性是否为 True，如无 Parent 则返回 False
function IsParentFont(AControl: TControl): Boolean;
begin
  try
    Result := not (AControl.Parent = nil);
    if Result then
      Result := TCnFontControl(AControl).ParentFont;
  except
    Result := False;
  end;
end;

// 取某 Control 的 Parent 的 Font 属性，如果没有返回 nil
function GetParentFont(AControl: TComponent): TFont;
begin
  Result := nil;
  try
    if AControl <> nil then
    begin
      if AControl is TControl then
      begin
        if TControl(AControl).Parent <> nil then
          Result := TCnFontControl(TControl(AControl).Parent).Font;
      end
      else if AControl is TComponent then
      begin
        if (AControl.Owner <> nil) and (AControl.Owner is TControl) then
          Result := TCnFontControl(AControl.Owner).Font;
      end;
    end;
  except
    ;
  end;
end;

{$ENDIF}

// 查找字符串在动态数组中的索引，用于 string 类型使用 Case 语句
function IndexStr(const AText: string; AValues: array of string; IgCase: Boolean = True): Integer;
type
  TSameFunc = function(const S1, S2: string): Boolean;
var
  Index: Integer;
  SameFunc: TSameFunc;
begin
  Result := -1;
  if IgCase then
    SameFunc := AnsiSameText
  else
    SameFunc := AnsiSameStr;

  for Index := Low(AValues) to High(AValues) do
    if SameFunc(AValues[Index], AText) then
    begin
      Result := Index;
      Exit;
    end;
end;

// 查找整形变量在动态数组中的索引，用于变量使用Case语句
function IndexInt(ANum: Integer; AValues: array of Integer): Integer;
var
  Index: Integer;
begin
  Result := -1;
  for Index := Low(AValues) to High(AValues) do
    if ANum = AValues[Index] then
    begin
      Result := Index;
      Exit;
    end;
end;

// 获取某集合内的元素数目，尺寸不对则返回 -1
function GetSetElementCount(const ASet; ASetSize: Integer): Integer;
var
  I, SetVal: Integer;
  S: TIntegerSet;
begin
  Result := -1;
  if (ASetSize <= 0) or (ASetSize > SizeOf(Integer)) then
    Exit;

  SetVal := 0;
  Move(ASet, SetVal, ASetSize);
  Integer(S) := SetVal;

  Result := 0;
  for I := 0 to SizeOf(Integer) * 8 - 1 do
  begin
    if I in S then
      Inc(Result);
  end;
end;

{$IFDEF MSWINDOWS}

// 将文件映射为内存文件并返回文件句柄、映射句柄与映射的内存起始地址，成功返回 True
function CnMapFileToPointer(const FileName: string; out FileHandle, MapHandle: THandle;
  out Address: Pointer): Boolean;
begin
  // 打开文件、创建映射、映射地址
  Result := False;
  FileHandle := CreateFile(PChar(FileName), GENERIC_READ, FILE_SHARE_READ or
                FILE_SHARE_WRITE, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL or
                FILE_FLAG_SEQUENTIAL_SCAN, 0);

  if FileHandle <> INVALID_HANDLE_VALUE then
  begin
    MapHandle := CreateFileMapping(FileHandle, nil, PAGE_READONLY, 0, 0, nil);
    if MapHandle <> 0 then
    begin
      Address := MapViewOfFile(MapHandle, FILE_MAP_READ, 0, 0, 0);
      if Address <> nil then
      begin
        Result := True; // 成功返回时，三个值都是有效的
        Exit;
      end
      else // 如果创建映射成功，但地址映射失败，就需要关闭创建映射
      begin
        CloseHandle(MapHandle);
        MapHandle := INVALID_HANDLE_VALUE;
      end;
    end
    else // 如果打开文件成功，但创建映射失败，就需要关闭文件
    begin
      CloseHandle(FileHandle);
      MapHandle := INVALID_HANDLE_VALUE;
    end;
  end;
end;

// 释放映射的内存文件，与 CnMapFileToPointer 对应
function CnUnMapFileFromPointer(var FileHandle, MapHandle: THandle;
  var Address: Pointer): Boolean;
begin
  UnmapViewOfFile(Address);
  Address := nil;

  CloseHandle(MapHandle);
  MapHandle := INVALID_HANDLE_VALUE;

  CloseHandle(FileHandle);
  FileHandle := INVALID_HANDLE_VALUE;

  Result := True;
end;

{$WARNINGS OFF}

// 处理一个 PE 文件，提取出其中第 DirectoryIndex 个 DataDirectory 的内容并写到流中，返回提取是否成功
function ExtractPEDataDirectory(const FileName: string; DirectoryIndex: Integer;
  OutStream: TStream): Boolean;
var
  F, M: THandle;
  P: Pointer;
  Ptr: PByte;
  DH: PImageDosHeader;
  NH: PImageNtHeaders;
begin
  Result := False;
  if (DirectoryIndex < 0) or (DirectoryIndex >= IMAGE_NUMBEROF_DIRECTORY_ENTRIES)
    or (OutStream = nil) then
    Exit;

  if CnMapFileToPointer(FileName, F, M, P) then
  begin
    try
      Ptr := PByte(P);

      DH := PImageDosHeader(Ptr);
      if DH^.e_magic <> IMAGE_DOS_SIGNATURE then
        Exit;

      Inc(Ptr, DH^._lfanew);

      NH := PImageNtHeaders(Ptr);
      if NH^.Signature <> IMAGE_NT_SIGNATURE then
        Exit;

      Ptr := PByte(P);
      Inc(Ptr, NH^.OptionalHeader.DataDirectory[DirectoryIndex].VirtualAddress);

      if NH^.OptionalHeader.DataDirectory[DirectoryIndex].Size > 0 then
        Result := OutStream.Write(Ptr^, NH^.OptionalHeader.DataDirectory[DirectoryIndex].Size)
          = NH^.OptionalHeader.DataDirectory[DirectoryIndex].Size;
    finally
      CnUnMapFileFromPointer(F, M, P);
    end;
  end;
end;

{$WARNINGS ON}

{$ENDIF}

// 从文件中载入字节数组，不进行编码转换
function LoadRawFileToBytes(const FileName: string): TBytes;
var
  F: TFileStream;
begin
  Result := nil;
  if not FileExists(FileName) then
    Exit;

  F := TFileStream.Create(FileName, fmOpenRead);
  try
    SetLength(Result, F.Size);
    F.Read(Result[0], F.Size);
  finally
    F.Free;
  end;
end;

// 将字节数组写入文件，不进行编码转换
procedure SaveRawFileFromBytes(B: TBytes; const FileName: string);
var
  F: TFileStream;
begin
  if B = nil then
    Exit;

  F := TFileStream.Create(FileName, fmCreate);
  try
    F.Write(B[0], Length(B));
  finally
    F.Free;
  end;
end;

function StrToBytes(const S: AnsiString): TBytes;
begin
  if S <> '' then
  begin
    SetLength(Result, Length(S));
    Move(S[1], Result[0], Length(S));
  end
  else
    Result := nil;
end;

function BytesToStr(Data: TBytes): AnsiString;
begin
  if Length(Data) > 0 then
  begin
    SetLength(Result, Length(Data));
    Move(Data[0], Result[1], Length(Data));
  end
  else
    Result := '';
end;

type
{$IFDEF UNICODE}
  TIdentString = type string;
{$ELSE}
  TIdentString = WideString;
{$ENDIF}

{$WARNINGS OFF}

function ConvertStringToIdent(const Str, Prefix: string; UseUnderLine: Boolean;
  IdentWordStyle: TCnIdentWordStyle; UseFullPinYin: Boolean;
  MaxWideChars, MaxWords, MaxCharLength: Integer): string;
const
  ALPHA_NUM = ['A'..'Z', 'a'..'z', '0'..'9'];
var
  WS, WD: TIdentString;
  Hz: AnsiString;
  CC, CW: Integer;
  P: PWideChar;
  AnsiBuilder, WideBuilder: TCnStringBuilder;

  function ProcessIdentStyle(const D: TIdentString): TIdentString;
  var
    I: Integer;
  begin
    case IdentWordStyle of
      iwsLowerCase:
        Result := LowerCase(D);
      iwsUpperCase:
        Result := UpperCase(D);
      iwsUpperFirstChar:
        begin
          Result := LowerCase(D);
          for I := 1 to Length(Result) do // 找到第一个小写字母的变成大写，避免数字开头
          begin
            if (Ord(Result[I]) < 128) and (AnsiChar(Result[I]) in ['a'..'z']) then
            begin
              Result[I] := WideChar(Ord(Result[I]) - 32);
              Exit;
            end;
          end;
        end;
    end;
  end;

  function IsHZChar(UC: WideChar): Boolean;
  var
    I: Integer;
  begin
    Result := Ord(UC) > SCN_UTF16_ANSI_WIDE_CHAR_SEP;
    if Result then
    begin
      for I := Low(SCN_CHINESE_SEP_CHARS) to High(SCN_CHINESE_SEP_CHARS) do
      begin
        if UC = SCN_CHINESE_SEP_CHARS[I] then
        begin
          Result := False;
          Exit;
        end;
      end;
    end;
  end;

begin
  // Ansi 和 Unicode 都得支持
  Result := Prefix;
  if Str = '' then
    Exit;
  if MaxCharLength < 2 then
    MaxCharLength := 64; // 默认 64

  AnsiBuilder := nil;
  WideBuilder := nil;

{$IFDEF UNICODE}
  WS := Str;
{$ELSE}
  WS := WideString(Str);
{$ENDIF};

  // 从头到尾扫描宽字符，字母数字就放过去，汉字就转成拼音放过去，
  try
    AnsiBuilder := TCnStringBuilder.Create(True);
    WideBuilder := TCnStringBuilder.Create(False);

    P := PWideChar(WS);
    CC := 0;
    CW := 0; // 俩计数

    while P^ <> #0 do
    begin
      WD := '';
      if IsHZChar(P^) then
      begin
        if UseFullPinYin then
        begin
          // 单个汉字，拿到完整拼音，处理好后放 WD 里
          WD := ProcessIdentStyle(GetPinYinFromUtf16Char(P^));
          Inc(P);
        end
        else
        begin
          // 当作汉字往后找一串，拿到拼音处理好后放 WD 里
          AnsiBuilder.Append(AnsiString(P^));
          Inc(P);
          while IsHZChar(P^) do
          begin
            AnsiBuilder.Append(AnsiString(P^));
            Inc(P);
          end;
{$IFDEF UNICODE}
          Hz := AnsiBuilder.ToAnsiString;
{$ELSE}
          Hz := AnsiBuilder.ToString;
{$ENDIF}
          WD := ProcessIdentStyle(GetHzPy(Hz)); // GetHzPy 只支持 AnsiString
        end;

        AnsiBuilder.Clear;
        if WD <> '' then
          Inc(CC);
      end
      else if (Ord(P^) < 128) and (AnsiChar(P^) in ALPHA_NUM) then
      begin
        // 是标识符，往后扫描至 #0 或非标识符
        WideBuilder.Append(P^);
        Inc(P);
        while (Ord(P^) < 128) and (AnsiChar(P^) in ALPHA_NUM) do
        begin
          WideBuilder.Append(P^);
          Inc(P);
        end;

        // 本 Word 扫描完毕，处理好后放 WD 里
{$IFDEF UNICODE}
        WD := ProcessIdentStyle(WideBuilder.ToString);
{$ELSE}
        WD := ProcessIdentStyle(WideBuilder.ToWideString);
{$ENDIF}
        WideBuilder.Clear;
        Inc(CW);
      end
      else
        Inc(P); // 其余情况当作分隔符

      if Length(WD) > 0 then
      begin
        if UseUnderLine and (Length(Result) > 0) then
          Result := Result + '_' + string(WD)
        else
          Result := Result + string(WD);
      end;

      if (CC > MaxWideChars) or (CW > MaxWords) or (Length(Result) > MaxCharLength) then
        Break;
    end;
  finally
    WideBuilder.Free;
    AnsiBuilder.Free;
  end;
end;

{$WARNINGS ON}

function Int32Len(IntPtr: Pointer): Integer;
var
  P: PDWORD;
begin
  Result := 0;
  if IntPtr = nil then
    Exit;

  P := PDWORD(IntPtr);
  while P^ <> 0 do
  begin
    Inc(Result);
    Inc(P);
  end;
end;

{ TCnWideMemIniFile }

constructor TCnWideMemIniFile.Create(const AFileName: string);
var
  WList: TCnWideStringList;
  List: TStringList;
begin
  inherited Create(AFileName);
  WList := nil;
  List := nil;
  try
    WList := TCnWideStringList.Create;
    WList.LoadFromFile(AFileName);
    List := TStringList.Create;
    List.Text := WList.Text;
    SetStrings(List);
  finally
    WList.Free;
    List.Free;
  end;
end;

procedure TCnWideMemIniFile.UpdateFile;
var
  WList: TCnWideStringList;
  List: TStringList;
begin
  WList := nil;
  List := nil;
  try
    List := TStringList.Create;
    GetStrings(List);
    WList := TCnWideStringList.Create;
    WList.Text := List.Text;
    WList.SaveToFile(FileName, wlfUtf8);
  finally
    WList.Free;
    List.Free;
  end;
end;

{$IFDEF MSWINDOWS}

initialization
  WndLong := GetWindowLong(Application.Handle, GWL_EXSTYLE);
  InitAPIs;

finalization
  if NtDllHandle <> 0 then
    FreeLibrary(NtDllHandle);

{$ENDIF}
end.


