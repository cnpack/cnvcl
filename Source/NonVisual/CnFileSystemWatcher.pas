{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2022 CnPack 开发组                       }
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
{            网站地址：http://www.cnpack.org                                   }
{            电子邮件：master@cnpack.org                                       }
{                                                                              }
{******************************************************************************}

unit CnFileSystemWatcher;
{* |<PRE>
================================================================================
* 软件名称：不可视工具组件包
* 单元名称：CnFileSystemWatcher 实现单元
* 单元作者：CnPack 开发组 Solokey
* 备    注：该单元实现了文件/目录变化的监视组件，可以监视到某目录（可包括子目录）
*           下的文件、目录发生的名称修改、大小改变、访问时间改变等、事件的发生。
*           监视选项可自行选择并组合。
* 开发平台：PWinXP + Delphi 5.0
* 兼容测试：PWin9X/2000/XP + Delphi 5/6
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 修改记录：2008.10.24 V1.2
*                 修正在受限用户模式下无法获取目录句柄的错误
*           2008.05.09 V1.1
*                增加通配符的处理
*           2007.11.03 V1.0
*                创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Windows, Classes, SysUtils,
  CnNative, CnCommon, CnClasses, CnConsts, CnCompConsts;

type
  TFileOperation = (foAdded, foRemoved, foModified, foRenamed);
  TFileDealMethod = procedure(Sender: TObject; FileOperation: TFileOperation; const FileName1,
    FileName2: string) of object;

  TNotifyFilter = (nfFileNameChange, nfDirNameChange, nfAttributeChange,
    nfSizeChange, nfWriteChange, nfAccessChange, nfCreationDateChange,
    nfSecurityChange);
  TNotifyFilters = set of TNotifyFilter;

  TNotificationBuffer =  array[0..4095] of Byte;

  PFileNotifyInformation = ^TFileNotifyInformation;
  TFileNotifyInformation = record
    NextEntryOffset: DWORD;
    Action: DWORD;
    FileNameLength: DWORD;
    FileName: array[0..0] of WideChar;
  end;

  TCnFileSystemWatcher = class;

  TCnShellChangeThread = class(TThread)
  private
    FParent: TCnFileSystemWatcher;
    FActive: Boolean;
    FDirectoryHandle: Cardinal;
    FCS: TRTLCriticalSection;
    FChangeEvent: TFileDealMethod;
    FDirectory: string;
    FWatchSubTree: Boolean;
    FFileMasks: TStringList;
    FTmpFileMasks: TStringList;
    FIncludePath: Boolean;
    FCompletionPort: Cardinal;
    FOverlapped: TOverlapped;
    FNotifyOptionFlags: DWORD;
    FBytesWritten: DWORD;
    FNotificationBuffer: TNotificationBuffer;
  protected
    procedure Execute; override;
    procedure DoIOCompletionEvent;
    function  ResetReadDirctory: Boolean;
    procedure Lock;
    procedure Unlock;
  public
    constructor Create(AParent: TCnFileSystemWatcher; ChangeEvent: TFileDealMethod); virtual;
    destructor Destroy; override;
    procedure SetDirectoryOptions(Directory : String; Active: Boolean; WatchSubTree : Boolean;
      NotifyOptionFlags : DWORD);
    procedure SetFileMasks(FileMasks: TStringList);
    procedure SetIncludePath(IncludePath: Boolean);
    property ChangeEvent : TFileDealMethod read FChangeEvent write FChangeEvent;
  end;

  TCnFileSystemWatcher = class(TCnComponent)
  private
    FActive: Boolean;
    FWatchedDir: string;
    FThread: TCnShellChangeThread;
    FOnChange: TFileDealMethod;
    FWatchSubTree: Boolean;
    FFilters: TNotifyFilters;
    FFileMasks: TStringList;
    FIncludePath: Boolean;
    procedure SetWatchedDir(const Value: string);
    procedure SetWatchSubTree(const Value: Boolean);
    procedure SetOnChange(const Value: TFileDealMethod);
    procedure SetFilters(const Value: TNotifyFilters);
    function  NotifyOptionFlags: DWORD;
    procedure SetActive(const Value: Boolean);
    procedure SetFileMasks(const Value: TStringList);
    procedure SetIncludePath(const Value: Boolean);
  protected
    procedure Change;
    procedure Start;
    procedure Stop;

    procedure OnFileMasksChange(Sender: TObject);

    procedure GetComponentInfo(var AName, Author, Email, Comment: string); override;
  public
    constructor Create(AOwner : TComponent); override;
    destructor  Destroy; override;
  published
    property Active: Boolean  read FActive write SetActive;
    property IncludePath: Boolean  read FIncludePath write SetIncludePath;
    property FileMasks: TStringList  read FFileMasks write SetFileMasks;
    property WatchedDir: string read FWatchedDir write SetWatchedDir;
    property WatchSubTree: Boolean read FWatchSubTree write SetWatchSubTree;
    property NotifyFilters: TNotifyFilters read FFilters write SetFilters;
    property OnChange: TFileDealMethod read FOnChange write SetOnChange;
  end;

implementation

{ TCnShellChangeThread }

constructor TCnShellChangeThread.Create(AParent: TCnFileSystemWatcher;
  ChangeEvent: TFileDealMethod);
begin
  FParent := AParent;
  FreeOnTerminate := True;
  FChangeEvent := ChangeEvent;
  InitializeCriticalSection(FCS);
  FDirectoryHandle := 0;
  FCompletionPort := 0;
  FFileMasks := TStringList.Create;
  FTmpFileMasks := TStringList.Create;
  inherited Create(True);
end;

destructor TCnShellChangeThread.Destroy;
begin
  FFileMasks.Free;
  FTmpFileMasks.Free;
  CloseHandle(FDirectoryHandle);
  CloseHandle(FCompletionPort);
  DeleteCriticalSection(FCS);
  inherited Destroy;
end;

procedure TCnShellChangeThread.DoIOCompletionEvent;
var
  TempBuffer: TNotificationBuffer;
  FileOpNotification: PFileNotifyInformation;
  Offset: Longint;
  FileName1, FileName2: string;
  FileOperation: TFileOperation;
  procedure DoDirChangeEvent;
  var
    IsInFileMasks: Boolean;
  begin
    if Assigned(ChangeEvent) and FActive then
    begin
      if FTmpFileMasks.Count > 0 then
        IsInFileMasks := FileMatchesMasks(FileName1, FTmpFileMasks)
      else
        IsInFileMasks := FileMatchesMasks(FileName1, '*.*', False);
      if IsInFileMasks then
      begin
        if FIncludePath then
        begin
          FileName1 := GetTrueFileName(FDirectory + FileName1);
          if FileOperation = foRenamed then
            FileName2 := GetTrueFileName(FDirectory + FileName2);
        end;
        ChangeEvent(FParent, FileOperation, FileName1, FileName2);
      end;
    end;
  end;
  function GetFileName(const FileName: PWideChar; FileNameLength: DWORD):string;
  begin
    Result := '';
    if Trim(FileName) <> '' then
      Result := WideCharLenToString(FileName, FileNameLength div SizeOf(WideChar));
  end;
begin
  Lock;
  TempBuffer := FNotificationBuffer;
  FTmpFileMasks.Assign(FFileMasks);
  FillChar(FNotificationBuffer, SizeOf(FNotificationBuffer), 0);
  Unlock;
  Pointer(FileOpNotification) := @TempBuffer[0];
  repeat
    with FileOpNotification^ do
    begin
      Offset := NextEntryOffset;
      FileName2 := '';
      case Action of
        FILE_ACTION_ADDED..FILE_ACTION_RENAMED_OLD_NAME:
        begin
          FileName1 := GetFileName(FileName, FileNameLength);
          FileOperation := TFileOperation(Action - 1);
          if Action <> FILE_ACTION_RENAMED_OLD_NAME then
            DoDirChangeEvent;
        end;
        FILE_ACTION_RENAMED_NEW_NAME:
        begin
          if FileOperation = foRenamed then
          begin
            FileName2 := GetFileName(FileName, FileNameLength);
            DoDirChangeEvent;
          end;
        end;
      end;
    end;
  Pointer(FileOpNotification) := Pointer(Integer(FileOpNotification) + OffSet);
  until Offset = 0;
end;

procedure TCnShellChangeThread.Execute;
var
  numBytes: DWORD;
  CompletionKey: TCnNativePointer;
  PFOverlapped: POverlapped;
  TempDirectoryHandle: Cardinal;
  TempCompletionPort: Cardinal;
begin
  TempCompletionPort := FCompletionPort;
  while not Terminated do
  begin
    Lock;
    TempDirectoryHandle := FDirectoryHandle;
    TempCompletionPort := FCompletionPort;
    Unlock;
    if TempDirectoryHandle > 0  then
    begin
      PFOverlapped := @FOverlapped;
      GetQueuedCompletionStatus(TempCompletionPort, numBytes, CompletionKey,
        PFOverlapped, INFINITE);
      if CompletionKey = Handle then
      begin
        Synchronize(DoIOCompletionEvent);
        FBytesWritten := 0;
        FillChar(FNotificationBuffer, SizeOf(FNotificationBuffer), 0);
        ReadDirectoryChanges(FDirectoryHandle, @FNotificationBuffer,
          SizeOf(FNotificationBuffer), FWatchSubTree, FNotifyOptionFlags,
          @FBytesWritten, @FOverlapped, nil);
      end;
    end;
  end;
  PostQueuedCompletionStatus(TempCompletionPort, numBytes, CompletionKey, PFOverlapped);
end;

procedure TCnShellChangeThread.Lock;
begin
  EnterCriticalSection(FCS);
end;

function TCnShellChangeThread.ResetReadDirctory: Boolean;
var
  TempHandle: Cardinal;
  TempCompletionPort: Cardinal;
begin
  Result := False;
  CloseHandle(FDirectoryHandle);
  PostQueuedCompletionStatus(FCompletionPort, 0, 0, nil);
  CloseHandle(FCompletionPort);

  TempHandle := CreateFile(PChar(FDirectory), GENERIC_READ, 
                            FILE_SHARE_READ or FILE_SHARE_WRITE or FILE_SHARE_DELETE,
                            nil, OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS
                            or FILE_FLAG_OVERLAPPED, 0);
  Lock;
  FDirectoryHandle := TempHandle;
  Unlock;

  if (TempHandle = INVALID_HANDLE_VALUE) or
     (GetLastError in [ERROR_FILE_NOT_FOUND, ERROR_PATH_NOT_FOUND, ERROR_ACCESS_DENIED]) then
  begin
    Lock;
    FDirectoryHandle := 0;
    FCompletionPort := 0;
    Unlock;
    Exit;
  end;

  TempCompletionPort := CreateIoCompletionPort(FDirectoryHandle, 0, Handle, 0);

  Lock;
  FCompletionPort := TempCompletionPort;
  Unlock;

  FBytesWritten := 0;
  FillChar(FNotificationBuffer, SizeOf(FNotificationBuffer), 0);
  Result := ReadDirectoryChanges(FDirectoryHandle, @FNotificationBuffer,
    SizeOf(FNotificationBuffer), FWatchSubTree, FNotifyOptionFlags, @FBytesWritten,
    @FOverlapped, nil);
end;

procedure TCnShellChangeThread.SetDirectoryOptions(Directory: String; Active: Boolean;
  WatchSubTree: Boolean;  NotifyOptionFlags : DWORD);
begin
  FWatchSubTree := WatchSubTree;
  FNotifyOptionFlags := NotifyOptionFlags;
  FDirectory := IncludeTrailingBackslash(Directory);
  FActive := Active;
  ResetReadDirctory;
end;

procedure TCnShellChangeThread.SetFileMasks(FileMasks: TStringList);
begin
  if Assigned(FileMasks) then
  begin
    FFileMasks.Assign(FileMasks);
  end else
    FFileMasks.Text := '*.*';
end;

procedure TCnShellChangeThread.SetIncludePath(IncludePath: Boolean);
begin
  FIncludePath := IncludePath;
end;

procedure TCnShellChangeThread.Unlock;
begin
  LeaveCriticalSection(FCS);
end;

{ TCnFileSystemWatcher }

procedure TCnFileSystemWatcher.Change;
begin
  if csDesigning in ComponentState then
    Exit;
  if Assigned(FThread) then
  begin
    FThread.SetDirectoryOptions(FWatchedDir, FActive, LongBool(FWatchSubTree), NotifyOptionFlags);
  end;
end;

constructor TCnFileSystemWatcher.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FActive := False;
  FWatchedDir := 'C:\';
  FFilters := [nfFilenameChange, nfDirNameChange];
  FWatchSubTree := True;
  FFileMasks := TStringList.Create;
  FFileMasks.OnChange := OnFileMasksChange;
  FFileMasks.Text := '*.*';
  FIncludePath := False;
  FOnChange := nil;
end;

destructor TCnFileSystemWatcher.Destroy;
begin
  FFileMasks.Free;
  if Assigned(FThread) then
    FThread.Terminate;
  inherited Destroy;
end;

procedure TCnFileSystemWatcher.GetComponentInfo(var AName, Author, Email,
  Comment: string);
begin
  AName := SCnFileSystemWatcherName;
  Author := SCnPack_solokey;
  Email := SCnPack_solokeyEmail;
  Comment := SCnFileSystemWatcherComment;
end;

function TCnFileSystemWatcher.NotifyOptionFlags: DWORD;
begin
  Result := 0;
  if nfFileNameChange in FFilters then
    Result := Result or FILE_NOTIFY_CHANGE_FILE_NAME;
  if nfDirNameChange in FFilters then
    Result := Result or FILE_NOTIFY_CHANGE_DIR_NAME;
  if nfSizeChange in FFilters then
    Result := Result or FILE_NOTIFY_CHANGE_SIZE;
  if nfAttributeChange in FFilters then
    Result := Result or FILE_NOTIFY_CHANGE_ATTRIBUTES;
  if nfWriteChange in FFilters then
    Result := Result or FILE_NOTIFY_CHANGE_LAST_WRITE;
  if nfAccessChange in FFilters then
    Result := Result or FILE_NOTIFY_CHANGE_LAST_ACCESS;
  if nfCreationDateChange in FFilters then
    Result := Result or FILE_NOTIFY_CHANGE_CREATION;
  if nfSecurityChange in FFilters then
    Result := Result or FILE_NOTIFY_CHANGE_SECURITY;
end;

procedure TCnFileSystemWatcher.OnFileMasksChange(Sender: TObject);
begin
  if Assigned(FThread) then
    FThread.SetFileMasks(FFileMasks);
end;

procedure TCnFileSystemWatcher.SetActive(const Value: Boolean);
begin
  if FActive <> Value then
  begin
    FActive := Value;
    Change;
    if FActive then
      Start
    else
      Stop;
  end;
end;

procedure TCnFileSystemWatcher.SetFileMasks(const Value: TStringList);
begin
  if Assigned(Value) then
    FFileMasks.Assign(Value);
  if Assigned(FThread) then
    FThread.SetFileMasks(FFileMasks);
end;

procedure TCnFileSystemWatcher.SetFilters(const Value: TNotifyFilters);
begin
  if FFilters <> Value then
  begin
    FFilters := Value;
    Change;
  end;
end;

procedure TCnFileSystemWatcher.SetIncludePath(const Value: Boolean);
begin
  FIncludePath := Value;
  if Assigned(FThread) then
    FThread.SetIncludePath(FIncludePath);
end;

procedure TCnFileSystemWatcher.SetOnChange(const Value: TFileDealMethod);
begin
  FOnChange := Value;
  if Assigned(FOnChange) and FActive then
    Start
  else
    Stop;
  Change;
end;

procedure TCnFileSystemWatcher.SetWatchedDir(const Value: string);
begin
  if not SameText(FWatchedDir, Value) then
  begin
    FWatchedDir := Value;
    Change;
  end;
end;

procedure TCnFileSystemWatcher.SetWatchSubTree(const Value: Boolean);
begin
  if FWatchSubTree <> Value then
  begin
    FWatchSubTree := Value;
    Change;
  end;
end;

procedure TCnFileSystemWatcher.Start;
begin
  if csDesigning in ComponentState then
    Exit;
  if Assigned(FOnChange) then
  begin
    FThread := TCnShellChangeThread.Create(Self, FOnChange);
    FThread.SetDirectoryOptions(FWatchedDir, FActive, LongBool(FWatchSubTree), NotifyOptionFlags);
    FThread.SetFileMasks(FFileMasks);
    FThread.SetIncludePath(FIncludePath);
    FThread.Resume;
  end;
end;

procedure TCnFileSystemWatcher.Stop;
begin
  if csDesigning in ComponentState then
    Exit;
  if Assigned(FThread) then
  begin
    FThread.Terminate;
    FThread := nil;
  end;
end;

end.
