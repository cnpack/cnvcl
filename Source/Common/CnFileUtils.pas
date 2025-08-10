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
  SysUtils, Classes;

type
  TCnFindFileCallBack = procedure(const FullFileName: string; const Info: TSearchRec;
    var FindAbort: Boolean) of object;
  {* 查找指定目录下文件的回调函数，FullFileName 是带路径的完整文件名}

  TCnFindDirCallBack = procedure(const SubDir: string) of object;
  {* 查找指定目录时进入子目录回调函数，SubDir 是不包括根搜索路径的相对子路径，并非完整路径}

function CnFindFile(const Path: string; const FileNamePattern: string = '*';
  FileProc: TCnFindFileCallBack = nil; DirProc: TCnFindDirCallBack = nil;
  IncludeSubDir: Boolean = True): Boolean;
{* 根据通配符查找指定目录下的文件，返回是否被中断}

implementation

{$IFDEF COMPILER5}
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
      Result := Result + {$IFDEF MSWINDOWS} '\'; {$ELSE} '/'; {$ENDIF};
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
            Continue; // 跳过符号链接

          if (Info.Attr and faDirectory) <> faDirectory then
          begin
            if Assigned(FileProc) then
              FileProc(APath + Info.FindData.cFileName, Info, FindAbort);
          end
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
              Continue; // 跳过符号链接

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

end.
