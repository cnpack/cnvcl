{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2015 CnPack 开发组                       }
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

{******************************************************************************}
{ Unit Note:                                                                   }
{    This file is derived from JclShell.pas                                    }
{    Project JEDI Code Library (JCL)                                           }
{                                                                              }
{ Original author:                                                             }
{    Marcel van Brakel  http://www.delphi-jedi.org/                            }
{******************************************************************************}

unit CnShellUtils;
{* |<PRE>
================================================================================
* 软件名称：CnWizards IDE 专家工具包
* 单元名称：Shell 工具单元
* 单元作者：Jcl Project
* 备    注：该单元移植自 Jcl 中的 JclShell 单元
* 开发平台：PWinXP SP2 + Delphi 5.01
* 兼容测试：PWin9X/2000/XP + Delphi 5/6/7 + C++Builder 5/6
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 单元标识：$Id$
* 修改记录：2005.07.14 V1.0
*               移植自 JclShell
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Windows, Messages, SysUtils, ActiveX, ComObj, ShellApi, ShlObj, CnCommon;

type
  TUnicodePath = array[0..MAX_PATH-1] of WideChar;

function PidlFree(var IdList: PItemIdList): Boolean;
function DriveToPidlBind(const DriveName: string; out Folder: IShellFolder): PItemIdList;
function PathToPidlBind(const FileName: string; out Folder: IShellFolder): PItemIdList;
function DisplayContextMenuPidl(const Handle: HWND; const Folder: IShellFolder;
  Item: PItemIdList; Pos: TPoint): Boolean;
function DisplayContextMenu(const Handle: HWND; const FileName: string;
  Pos: TPoint): Boolean;

implementation

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

function DriveToPidlBind(const DriveName: string; out Folder: IShellFolder): PItemIdList;
var
  Attr: ULONG;
  Eaten: ULONG;
  DesktopFolder: IShellFolder;
  Drives: PItemIdList;
  Path: TUnicodePath;
begin
  Result := nil;
  if Succeeded(SHGetDesktopFolder(DesktopFolder)) then
  begin
    if Succeeded(SHGetSpecialFolderLocation(0, CSIDL_DRIVES, Drives)) then
    begin
      if Succeeded(DesktopFolder.BindToObject(Drives, nil, IID_IShellFolder,
        Pointer(Folder))) then
      begin
{$IFDEF UNICODE_STRING}
        MultiByteToWideChar(CP_ACP, MB_PRECOMPOSED,
          PAnsiChar(AnsiString(IncludeTrailingBackslash(DriveName))), -1, Path, MAX_PATH);
{$ELSE}
        MultiByteToWideChar(CP_ACP, MB_PRECOMPOSED,
          PChar(IncludeTrailingBackslash(DriveName)), -1, Path, MAX_PATH);
{$ENDIF}
        if FAILED(Folder.ParseDisplayName(0, nil, Path, Eaten, Result,
          Attr)) then
        begin
          Folder := nil;
          // Failure probably means that this is not a drive. However, do not
          // call PathToPidlBind() because it may cause infinite recursion.
        end;
      end;
    end;
    PidlFree(Drives);
  end;
end;

function PathToPidlBind(const FileName: string; out Folder: IShellFolder): PItemIdList;
var
  Attr, Eaten: ULONG;
  PathIdList: PItemIdList;
  DesktopFolder: IShellFolder;
  Path, ItemName: TUnicodePath;
begin
  Result := nil;
{$IFDEF UNICODE_STRING}
  MultiByteToWideChar(CP_ACP, MB_PRECOMPOSED, PAnsiChar(AnsiString(_CnExtractFilePath(FileName))), -1, Path, MAX_PATH);
  MultiByteToWideChar(CP_ACP, MB_PRECOMPOSED, PAnsiChar(AnsiString(_CnExtractFileName(FileName))), -1, ItemName, MAX_PATH);
{$ELSE}
  MultiByteToWideChar(CP_ACP, MB_PRECOMPOSED, PChar(_CnExtractFilePath(FileName)), -1, Path, MAX_PATH);
  MultiByteToWideChar(CP_ACP, MB_PRECOMPOSED, PChar(_CnExtractFileName(FileName)), -1, ItemName, MAX_PATH);
{$ENDIF}

  if Succeeded(SHGetDesktopFolder(DesktopFolder)) then
  begin
    if Succeeded(DesktopFolder.ParseDisplayName(0, nil, Path, Eaten, PathIdList,
      Attr)) then
    begin
      if Succeeded(DesktopFolder.BindToObject(PathIdList, nil, IID_IShellFolder,
        Pointer(Folder))) then
      begin
        if FAILED(Folder.ParseDisplayName(0, nil, ItemName, Eaten, Result,
          Attr)) then
        begin
          Folder := nil;
          Result := DriveToPidlBind(FileName, Folder);
        end;
      end;
      PidlFree(PathIdList);
    end
    else
      Result := DriveToPidlBind(FileName, Folder);
  end;
end;

function MenuCallback(Wnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
var
  ContextMenu2: IContextMenu2;
begin
  case Msg of
    WM_CREATE:
      begin
        ContextMenu2 := IContextMenu2(PCreateStruct(lParam).lpCreateParams);
        SetWindowLong(Wnd, GWL_USERDATA, Longint(ContextMenu2));
        Result := DefWindowProc(Wnd, Msg, wParam, lParam);
      end;
    WM_INITMENUPOPUP:
      begin
        ContextMenu2 := IContextMenu2(GetWindowLong(Wnd, GWL_USERDATA));
        ContextMenu2.HandleMenuMsg(Msg, wParam, lParam);
        Result := 0;
      end;
    WM_DRAWITEM, WM_MEASUREITEM:
      begin
        ContextMenu2 := IContextMenu2(GetWindowLong(Wnd, GWL_USERDATA));
        ContextMenu2.HandleMenuMsg(Msg, wParam, lParam);
        Result := 1;
      end;
  else
    Result := DefWindowProc(Wnd, Msg, wParam, lParam);
  end;
end;

function CreateMenuCallbackWnd(const ContextMenu: IContextMenu2): HWND;
const
  IcmCallbackWnd = 'ICMCALLBACKWND';
var
  WndClass: TWndClass;
begin
  FillChar(WndClass, SizeOf(WndClass), #0);
  WndClass.lpszClassName := PChar(IcmCallbackWnd);
  WndClass.lpfnWndProc := @MenuCallback;
  WndClass.hInstance := HInstance;
  Windows.RegisterClass(WndClass);
  Result := CreateWindow(IcmCallbackWnd, IcmCallbackWnd, WS_POPUPWINDOW, 0,
    0, 0, 0, 0, 0, HInstance, Pointer(ContextMenu));
end;

function DisplayContextMenuPidl(const Handle: HWND; const Folder: IShellFolder;
  Item: PItemIdList; Pos: TPoint): Boolean;
var
  Cmd: Cardinal;
  ContextMenu: IContextMenu;
  ContextMenu2: IContextMenu2;
  Menu: HMENU;
  CommandInfo: TCMInvokeCommandInfo;
  CallbackWindow: HWND;
begin
  Result := False;
  if (Item = nil) or (Folder = nil) then
    Exit;
  Folder.GetUIObjectOf(Handle, 1, Item, IID_IContextMenu, nil,
    Pointer(ContextMenu));
  if ContextMenu <> nil then
  begin
    Menu := CreatePopupMenu;
    if Menu <> 0 then
    begin
      if Succeeded(ContextMenu.QueryContextMenu(Menu, 0, 1, $7FFF, CMF_EXPLORE)) then
      begin
        CallbackWindow := 0;
        if Succeeded(ContextMenu.QueryInterface(IContextMenu2, ContextMenu2)) then
        begin
          CallbackWindow := CreateMenuCallbackWnd(ContextMenu2);
        end;
        ClientToScreen(Handle, Pos);
        Cmd := Cardinal(TrackPopupMenu(Menu, TPM_LEFTALIGN or TPM_LEFTBUTTON or
          TPM_RIGHTBUTTON or TPM_RETURNCMD, Pos.X, Pos.Y, 0, CallbackWindow, nil));
        if Cmd <> 0 then
        begin
          FillChar(CommandInfo, SizeOf(CommandInfo), #0);
          CommandInfo.cbSize := SizeOf(TCMInvokeCommandInfo);
          CommandInfo.hwnd := Handle;
          CommandInfo.lpVerb := MakeIntResourceA(Cmd - 1);
          CommandInfo.nShow := SW_SHOWNORMAL;
          Result := Succeeded(ContextMenu.InvokeCommand(CommandInfo));
        end;
        if CallbackWindow <> 0 then
          DestroyWindow(CallbackWindow);
      end;
      DestroyMenu(Menu);
    end;
  end;
end;

function DisplayContextMenu(const Handle: HWND; const FileName: string;
  Pos: TPoint): Boolean;
var
  ItemIdList: PItemIdList;
  Folder: IShellFolder;
begin
  Result := False;
  ItemIdList := PathToPidlBind(FileName, Folder);
  if ItemIdList <> nil then
  begin
    Result := DisplayContextMenuPidl(Handle, Folder, ItemIdList, Pos);
    PidlFree(ItemIdList);
  end;
end;

end.
 