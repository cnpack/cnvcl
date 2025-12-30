{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2026 CnPack 开发组                       }
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

unit CnFileUtils;
{* |<PRE>
================================================================================
* 软件名称：开发包基础库
* 单元名称：文件相关基础库单元
* 单元作者：CnPack 开发组
* 备    注：该单元定义了组件包的文件操作相关的基础类库，要求足够跨平台
* 开发平台：PWin7 + Delphi 5.0
* 兼容测试：PWin7 + Delphi 5 ~ XE 等及 FPC 3
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 修改记录：2025.08.10 V1.0
*               创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes {$IFDEF COMPILER5}, Windows {$ENDIF};

type
  TCnFindFileCallBack = procedure(const FullFileName: string; const Info: TSearchRec;
    var FindAbort: Boolean) of object;
  {* 查找到指定目录下文件时的回调函数。

     参数：
       const FullFileName: string         - 带路径的完整文件名
       const Info: TSearchRec             - 文件搜索结构
       var FindAbort: Boolean             - 是否中断，赋值为 True 时表示中断

     返回值：（无）
  }

  TCnFindDirCallBack = procedure(const SubDir: string) of object;
  {* 查找指定目录时进入子目录的回调函数。

     参数：
       const SubDir: string               - 不包括根搜索路径的相对子路径，并非完整路径

     返回值：（无）
  }

function CnFindFile(const Path: string; const FileNamePattern: string = '*';
  FileProc: TCnFindFileCallBack = nil; DirProc: TCnFindDirCallBack = nil;
  IncludeSubDir: Boolean = True): Boolean;
{* 根据通配符查找指定目录下的文件，返回是否查找完成、未被中断。
   注意：DirProc 仅在 IncludeSubDir 为 True 时被回调，表示准备遍历该子目录。

   参数：
     const Path: string                   - 待查找的目录
     const FileNamePattern: string        - 文件名单个匹配的通配符，默认 * 表示匹配所有文件
     FileProc: TCnFindFileCallBack        - 查找到文件时的回调
     DirProc: TCnFindDirCallBack          - 进入子目录时的回调，注意子目录名无需匹配通配符
     IncludeSubDir: Boolean               - 查找时是否包含子目录，为 True 时才会调用子目录回调

   返回值：Boolean                        - 返回是否查找完成、未被中断   
}

{$IFDEF COMPILER5}

function DirectoryExists(const Directory: string): Boolean;
{* 弥补 Delphi 5 下没有 DirectoryExists 函数的问题}

function ForceDirectories(Dir: string): Boolean;
{* 弥补 Delphi 5 下没有 ForceDirectories 函数的问题}

{$ENDIF}

implementation

{$IFDEF COMPILER5}

resourcestring
  SCnErrorCannotCreateDir = 'Unable to Create Directory';

const
  faSymLink   = $00000040;

{$ENDIF}

function CnFindFile(const Path: string; const FileNamePattern: string;
  FileProc: TCnFindFileCallBack; DirProc: TCnFindDirCallBack;
  IncludeSubDir: Boolean): Boolean;
var
  AbortFlag: Boolean;

  function MakePath(const Dir: string): string;
  begin
    Result := Trim(Dir);
    if Result = '' then Exit;
    if not IsPathDelimiter(Result, Length(Result)) then
    begin
{$IFDEF MSWINDOWS}
      Result := Result + '\';
{$ELSE}
      Result := Result + '/';
{$ENDIF};
    end;
  end;

  procedure DoFindFile(const Path, SubPath: string; const FileNamePattern: string;
    FileProc: TCnFindFileCallBack; DirProc: TCnFindDirCallBack; bSub: Boolean;
    var FindAbort: Boolean);
  var
    APath: string;
    Info: TSearchRec;
    Succ: Integer;
  begin
    APath := MakePath(MakePath(Path) + SubPath);
    Succ := FindFirst(APath + FileNamePattern, faAnyFile - faVolumeID, Info);
    try
      while Succ = 0 do
      begin
        if (Info.Name <> '.') and (Info.Name <> '..') then
        begin
          if (Info.Attr and faSymLink) <> 0 then
          begin
            Succ := FindNext(Info);
            Continue; // 跳过符号链接
          end;

          if (Info.Attr and faDirectory) <> faDirectory then
          begin
            if Assigned(FileProc) then
            begin
{$IFDEF MSWINDOWS}
              FileProc(APath + Info.FindData.cFileName, Info, FindAbort);
{$ELSE}
              FileProc(APath + Info.Name, Info, FindAbort);
{$ENDIF}
            end;
          end;
        end;

        if FindAbort then
          Exit;
        Succ := FindNext(Info);
      end;
    finally
      SysUtils.FindClose(Info);
    end;

    if bSub then
    begin
      Succ := FindFirst(APath + '*', faAnyFile - faVolumeID, Info);
      try
        while Succ = 0 do
        begin
          if (Info.Name <> '.') and (Info.Name <> '..') and
            (Info.Attr and faDirectory = faDirectory) then
          begin
            if (Info.Attr and faSymLink) <> 0 then
            begin
              Succ := FindNext(Info);
              Continue; // 跳过符号链接
            end;

            if Assigned(DirProc) then
              DirProc(MakePath(SubPath) + Info.Name);
            DoFindFile(Path, MakePath(SubPath) + Info.Name, FileNamePattern, FileProc,
              DirProc, bSub, FindAbort);

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
  AbortFlag := False;
  DoFindFile(Path, '', FileNamePattern, FileProc, DirProc, IncludeSubDir, AbortFlag);
  Result := not AbortFlag;
end;

{$IFDEF COMPILER5}

function DirectoryExists(const Directory: string): Boolean;
var
  Code: Integer;
begin
  Code := GetFileAttributes(PChar(Directory));
  Result := (Code <> -1) and (FILE_ATTRIBUTE_DIRECTORY and Code <> 0);
end;

function IsPathDelimiter(const S: string; Index: Integer): Boolean;
begin
  Result := (Index > 0) and (Index <= Length(S)) and (S[Index] = '\')
    and (ByteType(S, Index) = mbSingleByte);
end;

function ExcludeTrailingPathDelimiter(const S: string): string;
begin
  Result := S;
  if IsPathDelimiter(Result, Length(Result)) then
    SetLength(Result, Length(Result)-1);
end;

function ForceDirectories(Dir: string): Boolean;
var
  E: EInOutError;
begin
  Result := True;
  if Dir = '' then
  begin
    E := EInOutError.CreateRes(@SCnErrorCannotCreateDir);
    E.ErrorCode := 3;
    raise E;
  end;
  Dir := ExcludeTrailingPathDelimiter(Dir);

  if (Length(Dir) < 3) or DirectoryExists(Dir)
    or (ExtractFilePath(Dir) = Dir) then Exit; // avoid 'xyz:\' problem.

  Result := ForceDirectories(ExtractFilePath(Dir)) and CreateDir(Dir);
end;

{$ENDIF}

end.
