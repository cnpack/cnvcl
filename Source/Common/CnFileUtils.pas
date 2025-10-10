{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     �й����Լ��Ŀ���Դ�������������                         }
{                   (C)Copyright 2001-2025 CnPack ������                       }
{                   ------------------------------------                       }
{                                                                              }
{            ���������ǿ�Դ��������������������� CnPack �ķ���Э������        }
{        �ĺ����·�����һ����                                                }
{                                                                              }
{            ������һ��������Ŀ����ϣ�������ã���û���κε���������û��        }
{        �ʺ��ض�Ŀ�Ķ������ĵ���������ϸ���������� CnPack ����Э�顣        }
{                                                                              }
{            ��Ӧ���Ѿ��Ϳ�����һ���յ�һ�� CnPack ����Э��ĸ��������        }
{        ��û�У��ɷ������ǵ���վ��                                            }
{                                                                              }
{            ��վ��ַ��https://www.cnpack.org                                  }
{            �����ʼ���master@cnpack.org                                       }
{                                                                              }
{******************************************************************************}

unit CnFileUtils;
{* |<PRE>
================================================================================
* ������ƣ�������������
* ��Ԫ���ƣ��ļ���ػ����ⵥԪ
* ��Ԫ���ߣ�CnPack ������
* ��    ע���õ�Ԫ��������������ļ�������صĻ�����⣬Ҫ���㹻��ƽ̨
* ����ƽ̨��PWin7 + Delphi 5.0
* ���ݲ��ԣ�PWin7 + Delphi 5 ~ XE �ȼ� FPC 3
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* �޸ļ�¼��2025.08.10 V1.0
*               ������Ԫ
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes;

type
  TCnFindFileCallBack = procedure(const FullFileName: string; const Info: TSearchRec;
    var FindAbort: Boolean) of object;
  {* ���ҵ�ָ��Ŀ¼���ļ�ʱ�Ļص�������

     ������
       const FullFileName: string         - ��·���������ļ���
       const Info: TSearchRec             - �ļ������ṹ
       var FindAbort: Boolean             - �Ƿ��жϣ���ֵΪ True ʱ��ʾ�ж�

     ����ֵ�����ޣ�
  }

  TCnFindDirCallBack = procedure(const SubDir: string) of object;
  {* ����ָ��Ŀ¼ʱ������Ŀ¼�Ļص�������

     ������
       const SubDir: string               - ������������·���������·������������·��

     ����ֵ�����ޣ�
  }

function CnFindFile(const Path: string; const FileNamePattern: string = '*';
  FileProc: TCnFindFileCallBack = nil; DirProc: TCnFindDirCallBack = nil;
  IncludeSubDir: Boolean = True): Boolean;
{* ����ͨ�������ָ��Ŀ¼�µ��ļ��������Ƿ������ɡ�δ���жϡ�
   ע�⣺DirProc ���� IncludeSubDir Ϊ True ʱ���ص�����ʾ׼����������Ŀ¼��

   ������
     const Path: string                   - �����ҵ�Ŀ¼
     const FileNamePattern: string        - �ļ�������ƥ���ͨ�����Ĭ�� * ��ʾƥ�������ļ�
     FileProc: TCnFindFileCallBack        - ���ҵ��ļ�ʱ�Ļص�
     DirProc: TCnFindDirCallBack          - ������Ŀ¼ʱ�Ļص���ע����Ŀ¼������ƥ��ͨ���
     IncludeSubDir: Boolean               - ����ʱ�Ƿ������Ŀ¼��Ϊ True ʱ�Ż������Ŀ¼�ص�

   ����ֵ��Boolean                        - �����Ƿ������ɡ�δ���ж�   
}

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
          begin
            Succ := FindNext(Info);
            Continue; // ������������
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
              Continue; // ������������
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

end.
