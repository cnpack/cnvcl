{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2024 CnPack 开发组                       }
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

unit CnMemorySearch;
{* |<PRE>
================================================================================
* 软件名称：不可视工具组件包
* 单元名称：跨进程内容搜索的组件实现单元
* 单元作者：CodeGame
* 备    注：给定特征码表对文件或者内存区域搜索出地址及相关信息逐一比对内存。
* 备    注：已知问题：搜索模块时只搜本进程模块，未能遍历其他进程的模块
* 开发平台：PWinXP + Delphi 2007
* 兼容测试：暂无
* 修改记录：2013.09.02 v1.0
*               移植单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

{$IFDEF WIN32}

uses
  Classes, SysUtils, Tlhelp32, Windows, CnNative;

type
  TSearchMethodList = (smlSearchMemory, smlSearchFile); //搜索方法
  TErrList = (elFileErr, elModuleErr); //错误类型

  TModuleInfo = packed record
    CodeHeadSize: Integer;
    CodeStartAddr: DWORD;
    CodeSize: Integer;
    CodeBaseAddr: DWORD;
  end;

  TResultType = (rtPointer, rtPointerData, rtPointerCall); //返回数据类型
  TtagData= array[0..15] of Byte; {标志内容 16byte}
  PModuleTag = ^TModuleTag;
  TModuleTag = packed record
    TagData:TtagData;{标志内容 16byte}
    Offset: Integer; {偏移地址}
    Len: Integer; {标志长度}
  end;

  PDataItem = ^TDataItem;
  TDataItem = packed record
    FileName: string[255];
    ConstStr: string[255];
    ModuleTag: array[0..4] of TModuleTag; {5组标志}
    TagCount: Integer; {ModuleTag标志数量}
    FileOffset: DWORD; {文件偏移地址}
    MemoryOffset: DWORD; {内存偏移地址}
    ResultType: TResultType; {返回数据类型 0:返回PointerData, 1:返回PointerCall}
    ResultData: DWORD; {返回的内存数据}
  end;

  TDataItemList = array of TDataItem;

  TSearchFindEvent = procedure(const Index: Integer; pData: TDataItem) of object;

  TSearchCompleteEvent = procedure(const Status: Boolean) of object;

  TSearchErrEvent = procedure(const Status: TErrList; Msg: string) of object;

  TCnMemorySearchThread = class;

  TCnMemorySearch = class(TComponent)
  private
    FSearchCallTH: TCnMemorySearchThread;
    FStartSearch: Boolean;
    FDirectory: string;
    FDataList: TDataItemList;
    FSearchMethod: TSearchMethodList;
    FSearchFind: TSearchFindEvent;
    FSearchComplete: TSearchCompleteEvent;
    FSearchErrEvent: TSearchErrEvent;
    procedure SetStartSearch(const Val: Boolean);
  protected
    procedure DoSearchFindEvent(const Index: Integer; pData: TDataItem);
    procedure DoSearchCompleteEvent(const Status: Boolean);
    procedure DoErrEvent(const Status: TErrList; Msg: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property DataList: TDataItemList read FDataList;
    procedure SetCount(const Count: Integer);
    function GetCount: Integer;
    property StartSearch: Boolean read FStartSearch write SetStartSearch;
  published
    property Directory: string read FDirectory write FDirectory;
    property SearchMethod: TSearchMethodList read FSearchMethod write FSearchMethod;
    property OnSearchFind: TSearchFindEvent read FSearchFind write FSearchFind;
    property OnSearchComplete: TSearchCompleteEvent read FSearchComplete write FSearchComplete;
    property OnSearchError: TSearchErrEvent read FSearchErrEvent write FSearchErrEvent;
  end;

  TCnMemorySearchThread = class(TThread)
  private
    FOwner: TCnMemorySearch;
    function GetModuleInfo(const aModuleName: string): TModuleInfo;
    function GetFileInfo(const aFileStream: TMemoryStream): TModuleInfo;
    function MemorySearch: Boolean;
    function FileSearch: Boolean;
  protected
    procedure Execute; override;
  public
    constructor Create(Suspended: Boolean; AOwner: TCnMemorySearch);
    destructor Destroy; override;
  end;

{$ENDIF}

implementation

{$IFDEF WIN32}

{ TCnMemorySearchThread }

procedure TCnMemorySearchThread.Execute;
var
  _SearchRet: Boolean;
begin
  { Place thread code here }
  _SearchRet := False;
  while not Terminated do
  begin
    Sleep(1);
    if not FOwner.FStartSearch then
      Continue;

    try
      case FOwner.SearchMethod of
        smlSearchMemory: _SearchRet := Self.MemorySearch; //搜索内存模块模式
        smlSearchFile: _SearchRet := Self.FileSearch; //搜索文件模式
      end;
    finally
      FOwner.DoSearchCompleteEvent(_SearchRet);
      Self.Terminate; //搜索完成
      FOwner.FStartSearch := False;
    end;
  end;
end;

function TCnMemorySearchThread.GetFileInfo(const aFileStream: TMemoryStream): TModuleInfo;
var
  _DosHead: IMAGE_DOS_HEADER;
  _NtHead: IMAGE_NT_HEADERS;
begin
  Result.CodeHeadSize := 0;
  Result.CodeStartAddr := 0;
  Result.CodeSize := 0;
  if not Assigned(aFileStream) then
    Exit;
  if aFileStream.Size < 0 then
    Exit;

  CopyMemory(@_DosHead, aFileStream.Memory, Sizeof(_DosHead));
  if _DosHead.e_magic = IMAGE_DOS_SIGNATURE then
  begin
    CopyMemory(@_NtHead, Pointer(TCnNativeInt(aFileStream.Memory) + _DosHead._lfanew), SizeOf(_NtHead));
    Result.CodeHeadSize := _NtHead.OptionalHeader.SizeOfHeaders;
    Result.CodeStartAddr := _NtHead.OptionalHeader.SizeOfHeaders + 1;
    Result.CodeSize := _NtHead.OptionalHeader.SizeOfCode;
    Result.CodeBaseAddr := _NtHead.OptionalHeader.ImageBase + _NtHead.OptionalHeader.BaseOfCode;
  end;
end;

function TCnMemorySearchThread.GetModuleInfo(const aModuleName: string): TModuleInfo;
var
  _ModuleSnap: Cardinal;
  _PId: DWORD;
  _Me32: MODULEENTRY32;
  _Handle: THandle;
  _lpr: TCnNativeUInt;
  _DosHead: IMAGE_DOS_HEADER;
  _NtHead: IMAGE_NT_HEADERS;
  LowModuleName: string;
begin
  Result.CodeHeadSize := 0;
  Result.CodeStartAddr := 0;
  Result.CodeSize := 0;
  _PId := 0;
  _ModuleSnap := CreateToolhelp32Snapshot(TH32CS_SNAPMODULE, _PId);
  if (_ModuleSnap <> INVALID_HANDLE_VALUE) then
  begin
    ZeroMemory(@_Me32, sizeof(MODULEENTRY32));
    _Me32.dwSize := sizeof(MODULEENTRY32);
    if (Module32First(_ModuleSnap, _Me32)) then
    begin
      LowModuleName := LowerCase(aModuleName);
      repeat
        if LowerCase(_Me32.szModule) = LowModuleName then
        begin
          _Handle := OpenProcess(PROCESS_VM_READ, True, GetCurrentProcessID);

          ReadProcessMemory(_Handle,
            _Me32.modBaseAddr,
            @_DosHead, Sizeof(_DosHead),
            _lpr);

          if _DosHead.e_magic = IMAGE_DOS_SIGNATURE then
          begin

            ReadProcessMemory(_Handle,
              Pointer(TCnNativeInt(_Me32.modBaseAddr) + _DosHead._lfanew),
              @_NtHead, SizeOf(_NtHead),
              _lpr);

            Result.CodeHeadSize := _NtHead.OptionalHeader.SizeOfHeaders;
            Result.CodeStartAddr := _NtHead.OptionalHeader.ImageBase + _NtHead.OptionalHeader.BaseOfCode;
            Result.CodeSize := _NtHead.OptionalHeader.BaseOfData;
          end;
          Break;
        end;
      until (Module32Next(_ModuleSnap, _Me32) = False)
    end;
  end;
end;

function TCnMemorySearchThread.MemorySearch: Boolean;
var
  _ItemIndex, _CodePosition, _TagIndex, _FindCount: Integer;
  _ModuleInfo: TModuleInfo;
  _FileName: string;
begin
  Result := False;
  if FOwner.FDataList = nil then
    Exit;
  if FOwner.GetCount < 1 then
    Exit;

  for _ItemIndex := 0 to FOwner.GetCount - 1 do //枚举所有待查数据表内数据
  begin
    _FileName := string(FOwner.FDataList[_ItemIndex].FileName); //内存搜索不需要全路径名
    _ModuleInfo := GetModuleInfo(_FileName); //取得模块信息
    if (_ModuleInfo.CodeStartAddr <= 0) or (_ModuleInfo.CodeSize <= 0) then
    begin
      FOwner.DoErrEvent(elModuleErr, Format('%s,模块打开错误!', [FOwner.FDataList[_ItemIndex].FileName]));
      Continue; //下一条数据
    end;

    for _CodePosition := 0 to _ModuleInfo.CodeSize - 1 do //从文件开始搜索
    begin
      _FindCount := 0; //查到标志数量
      for _TagIndex := 0 to FOwner.FDataList[_ItemIndex].TagCount - 1 do
      begin
        if CompareMem(Pointer(_ModuleInfo.CodeStartAddr + FOwner.FDataList[_ItemIndex].ModuleTag[_TagIndex].Offset + _CodePosition),
          @FOwner.FDataList[_ItemIndex].ModuleTag[_TagIndex], FOwner.FDataList[_ItemIndex].ModuleTag[_TagIndex].Len) then Inc(_FindCount);
      end;

      if _FindCount = FOwner.FDataList[_ItemIndex].TagCount then
      begin
        FOwner.FDataList[_ItemIndex].MemoryOffset := _ModuleInfo.CodeStartAddr + _CodePosition;
        FOwner.FDataList[_ItemIndex].FileOffset := FOwner.FDataList[_ItemIndex].MemoryOffset - _ModuleInfo.CodeStartAddr + _ModuleInfo.CodeHeadSize; //文件地址
        case FOwner.FDataList[_ItemIndex].ResultType of
          rtPointer: FOwner.FDataList[_ItemIndex].ResultData := FOwner.FDataList[_ItemIndex].MemoryOffset;
          rtPointerData: FOwner.FDataList[_ItemIndex].ResultData := PDword(FOwner.FDataList[_ItemIndex].MemoryOffset)^;
          rtPointerCall: FOwner.FDataList[_ItemIndex].ResultData := FOwner.FDataList[_ItemIndex].MemoryOffset + PInteger(FOwner.FDataList[_ItemIndex].MemoryOffset)^ + 4;
        end;
        FOwner.DoSearchFindEvent(_ItemIndex, FOwner.FDataList[_ItemIndex]);
        Break; //下条数据
      end;
    end;
  end;
  Result := True;
end;

function TCnMemorySearchThread.FileSearch: Boolean;
var
  _ItemIndex, _CodePosition, _TagIndex, _FindCount: Integer;
  _MemoryFile: TMemoryStream;
  _ModuleInfo: TModuleInfo;
  _FileName: string;
begin
  Result := False;
  if FOwner.FDataList = nil then
    Exit;
  if FOwner.GetCount < 1 then
    Exit;

  _MemoryFile := TMemoryStream.Create;
  try
    for _ItemIndex := 0 to FOwner.GetCount - 1 do //枚举所有待查数据表内数据
    begin
      _FileName := FOwner.Directory + string(FOwner.FDataList[_ItemIndex].FileName);
      if not FileExists(_FileName) then //文件不存在
      begin
        FOwner.DoErrEvent(elFileErr, Format('%s,文件打开错误!', [FOwner.FDataList[_ItemIndex].FileName]));
        Continue; //下一条数据
      end;
      _MemoryFile.LoadFromFile(_FileName);
      _ModuleInfo := GetFileInfo(_MemoryFile); //取得模块信息
      if (_ModuleInfo.CodeSize = 0) or (_ModuleInfo.CodeStartAddr = 0) then
      begin
        FOwner.DoErrEvent(elFileErr, Format('%s,文件格式错误!', [FOwner.FDataList[_ItemIndex].FileName]));
        Continue; //下一条数据
      end;
      _MemoryFile.Position := 0;

      for _CodePosition := 0 to _ModuleInfo.CodeSize - 1 do //从文件开始搜索
      begin
        _FindCount := 0; //查到标志数量
        for _TagIndex := 0 to FOwner.FDataList[_ItemIndex].TagCount - 1 do
        begin
          if CompareMem(Pointer(DWORD(_MemoryFile.Memory) + _ModuleInfo.CodeStartAddr + FOwner.FDataList[_ItemIndex].ModuleTag[_TagIndex].Offset + _CodePosition),
            @FOwner.FDataList[_ItemIndex].ModuleTag[_TagIndex], FOwner.FDataList[_ItemIndex].ModuleTag[_TagIndex].Len) then Inc(_FindCount);
        end;

        if _FindCount = FOwner.FDataList[_ItemIndex].TagCount then
        begin
          FOwner.FDataList[_ItemIndex].FileOffset := _ModuleInfo.CodeStartAddr + _CodePosition;
          FOwner.FDataList[_ItemIndex].MemoryOffset := FOwner.FDataList[_ItemIndex].FileOffset + _ModuleInfo.CodeBaseAddr - _ModuleInfo.CodeHeadSize;

          case FOwner.FDataList[_ItemIndex].ResultType of
            rtPointer: FOwner.FDataList[_ItemIndex].ResultData := FOwner.FDataList[_ItemIndex].MemoryOffset;
            rtPointerData: FOwner.FDataList[_ItemIndex].ResultData := PDWord(FOwner.FDataList[_ItemIndex].FileOffset + DWORD(_MemoryFile.Memory))^;
            rtPointerCall: FOwner.FDataList[_ItemIndex].ResultData := FOwner.FDataList[_ItemIndex].MemoryOffset + PInteger(FOwner.FDataList[_ItemIndex].FileOffset + DWORD(_MemoryFile.Memory))^ + 4;
          end;
          FOwner.DoSearchFindEvent(_ItemIndex, FOwner.FDataList[_ItemIndex]);
          Break; //下条数据
        end;
      end;
    end;

    Result := True;
  finally
    FreeAndNil(_MemoryFile);
  end;

end;

constructor TCnMemorySearchThread.Create(Suspended: Boolean; AOwner: TCnMemorySearch);
begin
  inherited Create(Suspended);
  FreeOnTerminate := True;
  FOwner := AOwner;
end;

destructor TCnMemorySearchThread.Destroy;
begin
  {.....}
  FOwner := nil;
  inherited Destroy;
end;

{ TCnMemorySearch }

constructor TCnMemorySearch.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

end;

destructor TCnMemorySearch.Destroy;
begin

  inherited Destroy;
end;

procedure TCnMemorySearch.DoErrEvent(const Status: TErrList; Msg: string);
begin
  if Assigned(FSearchErrEvent) then
  try
    FSearchErrEvent(Status, Msg);
  except
    ;
  end;
end;

procedure TCnMemorySearch.DoSearchCompleteEvent(const Status: Boolean);
begin
  if Assigned(FSearchComplete) then
  try
    FSearchComplete(Status);
  except
    ;
  end;
end;

procedure TCnMemorySearch.DoSearchFindEvent(const Index: Integer; pData: TDataItem);
begin
  if Assigned(FSearchFind) then
  try
    FSearchFind(Index, pData);
  except
    ;
  end;
end;

function TCnMemorySearch.GetCount: Integer;
begin
  Result := Length(FDataList);
end;

procedure TCnMemorySearch.SetCount(const Count: Integer);
begin
  SetLength(FDataList, Count);
end;

procedure TCnMemorySearch.SetStartSearch(const Val: Boolean);
begin
//  if FStartSearch = Val then Exit;
  if Val then
  begin
    if Assigned(FSearchCallTH) then
      FSearchCallTH.Terminate;
    FSearchCallTH := TCnMemorySearchThread.Create(False, Self);
  end
  else
  begin
    if Assigned(FSearchCallTH) then
      FSearchCallTH.Terminate;
    FSearchCallTH := nil;
  end;
  FStartSearch := Val;
end;

{$ENDIF}
end.

