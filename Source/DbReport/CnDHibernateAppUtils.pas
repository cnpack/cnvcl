{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2021 CnPack 开发组                       }
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

unit CnDHibernateAppUtils; 
{* |<PRE>
================================================================================
* 软件名称：CnDHibernate基础库
* 单元名称：工程相关函数库
* 单元作者：Rarnu (rarnu@cnpack.org)
* 备    注：
* 开发平台：PWinXP SP2 + Delphi 2009
* 兼容测试：Win2000/XP/Vista/2008 + Delphi 2009
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 修改记录：2008.08.23 V1.8
*               移植到 Delphi2009
*           2006.09.04 V1.0
*               创建单元
================================================================================
|</PRE>}

{$HINTS OFF}

interface

{$I CnPack.inc}

{$IFDEF SUPPORT_ADO}

uses
  Windows, Registry, Classes, Controls, Forms, IniFiles, Grids, SysUtils,
  Messages, Consts, RTLConsts, CnDHibernateConsts;

function GetDefaultSection(Component: TComponent): string;

function GetDefaultIniName: string;

type
  TCnOnGetDefaultIniName = function: string;

const
  OnGetDefaultIniName: TCnOnGetDefaultIniName = nil;

var
  DefCompanyName: string = '';
  RegUseAppTitle: Boolean = False;

function GetDefaultIniRegKey: string;

function FindForm(FormClass: TFormClass): TForm;

function FindShowForm(FormClass: TFormClass; const Caption: string): TForm;

function ShowDialog(FormClass: TFormClass): Boolean;

function InstantiateForm(FormClass: TFormClass; var Reference): TForm;

procedure SaveFormPlacement(Form: TForm; const IniFileName: string; UseRegistry: Boolean);

procedure RestoreFormPlacement(Form: TForm; const IniFileName: string; UseRegistry: Boolean);

procedure WriteFormPlacementReg(Form: TForm; IniFile: TRegIniFile; const Section: string);

procedure ReadFormPlacementReg(Form: TForm; IniFile: TRegIniFile; const Section: string; LoadState, LoadPosition: Boolean);

procedure SaveMDIChildrenReg(MainForm: TForm; IniFile: TRegIniFile);

procedure RestoreMDIChildrenReg(MainForm: TForm; IniFile: TRegIniFile);

procedure RestoreGridLayoutReg(Grid: TCustomGrid; IniFile: TRegIniFile);

procedure SaveGridLayoutReg(Grid: TCustomGrid; IniFile: TRegIniFile);

procedure WriteFormPlacement(Form: TForm; IniFile: TIniFile; const Section: string);

procedure ReadFormPlacement(Form: TForm; IniFile: TIniFile; const Section: string; LoadState, LoadPosition: Boolean);

procedure SaveMDIChildren(MainForm: TForm; IniFile: TIniFile);

procedure RestoreMDIChildren(MainForm: TForm; IniFile: TIniFile);

procedure RestoreGridLayout(Grid: TCustomGrid; IniFile: TIniFile);

procedure SaveGridLayout(Grid: TCustomGrid; IniFile: TIniFile);

function GetUniqueFileNameInDir(const Path, FileNameMask: string): string;

function StrToIniStr(const Str: string): string;

function IniStrToStr(const Str: string): string;

function IniReadString(IniFile: TObject; const Section, Ident, Default: string): string;

procedure IniWriteString(IniFile: TObject; const Section, Ident, Value: string);

function IniReadInteger(IniFile: TObject; const Section, Ident: string; Default: Longint): Longint;

procedure IniWriteInteger(IniFile: TObject; const Section, Ident: string; Value: Longint);

function IniReadBool(IniFile: TObject; const Section, Ident: string; Default: Boolean): Boolean;

procedure IniWriteBool(IniFile: TObject; const Section, Ident: string; Value: Boolean);

procedure IniReadSections(IniFile: TObject; Strings: TStrings);

procedure IniEraseSection(IniFile: TObject; const Section: string);

procedure IniDeleteKey(IniFile: TObject; const Section, Ident: string);

procedure AppBroadcast(Msg, wParam: Longint; lParam: Longint);

procedure AppTaskbarIcons(AppOnly: Boolean);

{ Internal using utilities }

procedure InternalSaveGridLayout(Grid: TCustomGrid; IniFile: TObject; const Section: string);

procedure InternalRestoreGridLayout(Grid: TCustomGrid; IniFile: TObject; const Section: string);

procedure InternalSaveMDIChildren(MainForm: TForm; IniFile: TObject);

procedure InternalRestoreMDIChildren(MainForm: TForm; IniFile: TObject);

{$ENDIF SUPPORT_ADO}

implementation

{$IFDEF SUPPORT_ADO}

uses
  CnCommon, CnDHibernateUtils, CnDHibernateStringUtils;

function GetDefaultSection(Component: TComponent): string;
var
  F: TCustomForm;
  Owner: TComponent;
begin
  if Component <> nil then
  begin
    if Component is TCustomForm then
      Result := Component.ClassName
    else
    begin
      Result := Component.Name;
      if Component is TControl then
      begin
        F := GetParentForm(TControl(Component));
        if F <> nil then
          Result := F.ClassName + Result
        else
        begin
          if TControl(Component).Parent <> nil then
            Result := TControl(Component).Parent.Name + Result;
        end;
      end
      else
      begin
        Owner := Component.Owner;
        if Owner is TForm then
          Result := Format('%s.%s', [Owner.ClassName, Result]);
      end;
    end;
  end
  else
    Result := EmptyStr;
end;

function GetDefaultIniName: string;
begin
  if Assigned(OnGetDefaultIniName) then
    Result := OnGetDefaultIniName
  else
    Result := _CnExtractFileName(_CnChangeFileExt(Application.ExeName, '.INI'));
end;

function GetDefaultIniRegKey: string;
begin
  if RegUseAppTitle and (Application.Title <> EmptyStr) then
    Result := Application.Title
  else
    Result := _CnExtractFileName(_CnChangeFileExt(Application.ExeName, EmptyStr));
  if DefCompanyName <> EmptyStr then
    Result := DefCompanyName + '\' + Result;
  Result := 'Software\' + Result;
end;

function FindForm(FormClass: TFormClass): TForm;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Screen.FormCount - 1 do
  begin
    if Screen.Forms[I] is FormClass then
    begin
      Result := Screen.Forms[I];
      Break;
    end;
  end;
end;

function InternalFindShowForm(FormClass: TFormClass; const Caption: string; Restore: Boolean): TForm;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Screen.FormCount - 1 do
  begin
    if Screen.Forms[I] is FormClass then
      if (Caption = EmptyStr) or (Caption = Screen.Forms[I].Caption) then
      begin
        Result := Screen.Forms[I];
        Break;
      end;
  end;
  if Result = nil then
  begin
    Application.CreateForm(FormClass, Result);
    if Caption <> '' then
      Result.Caption := Caption;
  end;
  with Result do
  begin
    if Restore and (WindowState = wsMinimized) then
      WindowState := wsNormal;
    Show;
  end;
end;

function FindShowForm(FormClass: TFormClass; const Caption: string): TForm;
begin
  Result := InternalFindShowForm(FormClass, Caption, True);
end;

function ShowDialog(FormClass: TFormClass): Boolean;
var
  Dlg: TForm;
begin
  Application.CreateForm(FormClass, Dlg);
  try
    Result := Dlg.ShowModal in [mrOk, mrYes];
  finally
    Dlg.Free;
  end;
end;

function InstantiateForm(FormClass: TFormClass; var Reference): TForm;
begin
  if TForm(Reference) = nil then
    Application.CreateForm(FormClass, Reference);
  Result := TForm(Reference);
end;

function StrToIniStr(const Str: string): string;
var
  Buffer: array[0..4095] of Char;
  B, S: PChar;
begin
  if Length(Str) > SizeOf(Buffer) then
    raise Exception.Create(ResStr(SLineTooLong));
  S := PChar(Str);
  try
    B := Buffer;
    while S^ <> #0 do
      case S^of
        #13, #10:
          begin
            if (S^ = #13) and (S[1] = #10) then
              Inc(S)
            else if (S^ = #10) and (S[1] = #13) then
              Inc(S);
            B^ := '\';
            Inc(B);
            B^ := 'n';
            Inc(B);
            Inc(S);
          end;
      else
        B^ := S^;
        Inc(B);
        Inc(S);
      end;
  finally

  end;
  B^ := #0;
  Result := StrPas(Buffer);
end;

function IniStrToStr(const Str: string): string;
var
  Buffer: array[0..4095] of Char;
  B, S: PChar;
begin
  if Length(Str) > SizeOf(Buffer) then
    raise Exception.Create(ResStr(SLineTooLong));
  S := PChar(Str);
  try
    B := Buffer;
    while S^ <> #0 do
      if (S[0] = '\') and (S[1] = 'n') then
      begin
        B^ := #13;
        Inc(B);
        B^ := #10;
        Inc(B);
        Inc(S);
        Inc(S);
      end
      else
      begin
        B^ := S^;
        Inc(B);
        Inc(S);
      end;
  finally

  end;
  B^ := #0;
  Result := StrPas(Buffer);
end;

function IniReadString(IniFile: TObject; const Section, Ident, Default: string): string;
begin
  if IniFile is TRegIniFile then
    Result := TRegIniFile(IniFile).ReadString(Section, Ident, Default)
  else if IniFile is TIniFile then
    Result := TIniFile(IniFile).ReadString(Section, Ident, Default)
  else
    Result := Default;
end;

procedure IniWriteString(IniFile: TObject; const Section, Ident, Value: string);
var
  S: string;
begin
  if IniFile is TRegIniFile then
    TRegIniFile(IniFile).WriteString(Section, Ident, Value)
  else
  begin
    S := Value;
    if S <> '' then
    begin
      if ((S[1] = '"') and (S[Length(S)] = '"')) or ((S[1] = '''') and (S[Length(S)] = '''')) then
        S := '"' + S + '"';
    end;
    if IniFile is TIniFile then
      TIniFile(IniFile).WriteString(Section, Ident, S);
  end;
end;

function IniReadInteger(IniFile: TObject; const Section, Ident: string; Default: Longint): Longint;
begin
  if IniFile is TRegIniFile then
    Result := TRegIniFile(IniFile).ReadInteger(Section, Ident, Default)
  else if IniFile is TIniFile then
    Result := TIniFile(IniFile).ReadInteger(Section, Ident, Default)
  else
    Result := Default;
end;

procedure IniWriteInteger(IniFile: TObject; const Section, Ident: string; Value: Longint);
begin
  if IniFile is TRegIniFile then
    TRegIniFile(IniFile).WriteInteger(Section, Ident, Value)
  else if IniFile is TIniFile then
    TIniFile(IniFile).WriteInteger(Section, Ident, Value);
end;

function IniReadBool(IniFile: TObject; const Section, Ident: string; Default: Boolean): Boolean;
begin
  if IniFile is TRegIniFile then
    Result := TRegIniFile(IniFile).ReadBool(Section, Ident, Default)
  else if IniFile is TIniFile then
    Result := TIniFile(IniFile).ReadBool(Section, Ident, Default)
  else
    Result := Default;
end;

procedure IniWriteBool(IniFile: TObject; const Section, Ident: string; Value: Boolean);
begin
  if IniFile is TRegIniFile then
    TRegIniFile(IniFile).WriteBool(Section, Ident, Value)
  else if IniFile is TIniFile then
    TIniFile(IniFile).WriteBool(Section, Ident, Value);
end;

procedure IniEraseSection(IniFile: TObject; const Section: string);
begin
  if IniFile is TRegIniFile then
    TRegIniFile(IniFile).EraseSection(Section)
  else if IniFile is TIniFile then
    TIniFile(IniFile).EraseSection(Section);
end;

procedure IniDeleteKey(IniFile: TObject; const Section, Ident: string);
var
  CSection: array[0..127] of Char;
  CIdent: array[0..127] of Char;
  CFileName: array[0..127] of Char;
begin
  if IniFile is TRegIniFile then
    TRegIniFile(IniFile).DeleteKey(Section, Ident)
  else if IniFile is TIniFile then
    TIniFile(IniFile).DeleteKey(Section, Ident);
end;

procedure IniReadSections(IniFile: TObject; Strings: TStrings);
begin
  if IniFile is TIniFile then
    TIniFile(IniFile).ReadSections(Strings)
  else if IniFile is TRegIniFile then
    TRegIniFile(IniFile).ReadSections(Strings);
end;

procedure InternalSaveMDIChildren(MainForm: TForm; IniFile: TObject);
var
  I: Integer;
begin
  if (MainForm = nil) or (MainForm.FormStyle <> fsMDIForm) then
    raise EInvalidOperation.Create(ResStr(SNoMDIForm));
  IniEraseSection(IniFile, DH_MDI_CHILD);
  if MainForm.MDIChildCount > 0 then
  begin
    IniWriteInteger(IniFile, DH_MDI_CHILD, DH_LIST_COUNT, MainForm.MDIChildCount);
    for I := 0 to MainForm.MDIChildCount - 1 do
      IniWriteString(IniFile, DH_MDI_CHILD, Format(DH_ITEM, [I]), MainForm.MDIChildren[I].ClassName);
  end;
end;

procedure InternalRestoreMDIChildren(MainForm: TForm; IniFile: TObject);
var
  I: Integer;
  Count: Integer;
  FormClass: TFormClass;
begin
  if (MainForm = nil) or (MainForm.FormStyle <> fsMDIForm) then
    raise EInvalidOperation.Create(ResStr(SNoMDIForm));
  StartWait;
  try
    Count := IniReadInteger(IniFile, DH_MDI_CHILD, DH_LIST_COUNT, 0);
    if Count > 0 then
    begin
      for I := 0 to Count - 1 do
      begin
        FormClass := TFormClass(GetClass(IniReadString(IniFile, DH_MDI_CHILD, Format(DH_ITEM, [Count - I - 1]), EmptyStr)));
        if FormClass <> nil then
          InternalFindShowForm(FormClass, EmptyStr, False);
      end;
    end;
  finally
    StopWait;
  end;
end;

procedure SaveMDIChildrenReg(MainForm: TForm; IniFile: TRegIniFile);
begin
  InternalSaveMDIChildren(MainForm, IniFile);
end;

procedure RestoreMDIChildrenReg(MainForm: TForm; IniFile: TRegIniFile);
begin
  InternalRestoreMDIChildren(MainForm, IniFile);
end;

procedure SaveMDIChildren(MainForm: TForm; IniFile: TIniFile);
begin
  InternalSaveMDIChildren(MainForm, IniFile);
end;

procedure RestoreMDIChildren(MainForm: TForm; IniFile: TIniFile);
begin
  InternalRestoreMDIChildren(MainForm, IniFile);
end;

procedure InternalSaveGridLayout(Grid: TCustomGrid; IniFile: TObject; const Section: string);
var
  I: Longint;
begin
  for I := 0 to TDrawGrid(Grid).ColCount - 1 do
    IniWriteInteger(IniFile, Section, Format(DH_ITEM, [I]), TDrawGrid(Grid).ColWidths[I]);
end;

procedure InternalRestoreGridLayout(Grid: TCustomGrid; IniFile: TObject; const Section: string);
var
  I: Longint;
begin
  for I := 0 to TDrawGrid(Grid).ColCount - 1 do
    TDrawGrid(Grid).ColWidths[I] := IniReadInteger(IniFile, Section, Format(DH_ITEM, [I]), TDrawGrid(Grid).ColWidths[I]);
end;

procedure RestoreGridLayoutReg(Grid: TCustomGrid; IniFile: TRegIniFile);
begin
  InternalRestoreGridLayout(Grid, IniFile, GetDefaultSection(Grid));
end;

procedure SaveGridLayoutReg(Grid: TCustomGrid; IniFile: TRegIniFile);
begin
  InternalSaveGridLayout(Grid, IniFile, GetDefaultSection(Grid));
end;

procedure RestoreGridLayout(Grid: TCustomGrid; IniFile: TIniFile);
begin
  InternalRestoreGridLayout(Grid, IniFile, GetDefaultSection(Grid));
end;

procedure SaveGridLayout(Grid: TCustomGrid; IniFile: TIniFile);
begin
  InternalSaveGridLayout(Grid, IniFile, GetDefaultSection(Grid));
end;

function CrtResString: string;
begin
  Result := Format('(%dx%d)', [GetSystemMetrics(SM_CXSCREEN), GetSystemMetrics(SM_CYSCREEN)]);
end;

function ReadPosStr(IniFile: TObject; const Section, Ident: string): string;
begin
  Result := IniReadString(IniFile, Section, Ident + CrtResString, EmptyStr);
  if Result = EmptyStr then
    Result := IniReadString(IniFile, Section, Ident, EmptyStr);
end;

procedure WritePosStr(IniFile: TObject; const Section, Ident, Value: string);
begin
  IniWriteString(IniFile, Section, Ident + CrtResString, Value);
  IniWriteString(IniFile, Section, Ident, Value);
end;

procedure InternalWriteFormPlacement(Form: TForm; IniFile: TObject; const Section: string);
var
  Placement: TWindowPlacement;
begin
  Placement.Length := SizeOf(TWindowPlacement);
  GetWindowPlacement(Form.Handle, @Placement);
  with Placement, TForm(Form) do
  begin
    if (Form = Application.MainForm) and IsIconic(Application.Handle) then
      ShowCmd := SW_SHOWMINIMIZED;
    if (FormStyle = fsMDIChild) and (WindowState = wsMinimized) then
      Flags := Flags or WPF_SETMINPOSITION;
    IniWriteInteger(IniFile, Section, DH_FLAGS, Flags);
    IniWriteInteger(IniFile, Section, DH_SHOW_CMD, ShowCmd);
    IniWriteInteger(IniFile, Section, DH_PIXELS, Screen.PixelsPerInch);
    WritePosStr(IniFile, Section, DH_MIN_MAX_POS, Format('%d,%d,%d,%d', [ptMinPosition.X, ptMinPosition.Y, ptMaxPosition.X, ptMaxPosition.Y]));
    WritePosStr(IniFile, Section, DH_NORM_POS, Format('%d,%d,%d,%d', [rcNormalPosition.Left, rcNormalPosition.Top, rcNormalPosition.Right, rcNormalPosition.Bottom]));
  end;
end;

procedure WriteFormPlacementReg(Form: TForm; IniFile: TRegIniFile; const Section: string);
begin
  InternalWriteFormPlacement(Form, IniFile, Section);
end;

procedure WriteFormPlacement(Form: TForm; IniFile: TIniFile; const Section: string);
begin
  InternalWriteFormPlacement(Form, IniFile, Section);
end;

procedure SaveFormPlacement(Form: TForm; const IniFileName: string; UseRegistry: Boolean);
var
  IniFile: TObject;
begin
  if UseRegistry then
    IniFile := TRegIniFile.Create(IniFileName)
  else
    IniFile := TIniFile.Create(IniFileName);
  try
    InternalWriteFormPlacement(Form, IniFile, Form.ClassName);
  finally
    IniFile.Free;
  end;
end;

type
  TCnNastyForm = class(TScrollingWinControl)
  private
    FActiveControl: TWinControl;
    FFocusedControl: TWinControl;
    FBorderIcons: TBorderIcons;
    FBorderStyle: TFormBorderStyle;
    FSizeChanging: Boolean;
    FWindowState: TWindowState;
  end;

  TCnHackComponent = class(TComponent);

procedure InternalReadFormPlacement(Form: TForm; IniFile: TObject; const Section: string; LoadState, LoadPosition: Boolean);
const
  Delims =[',', ' '];
var
  PosStr: string;
  Placement: TWindowPlacement;
  WinState: TWindowState;
  DataFound: Boolean;
begin
  if not (LoadState or LoadPosition) then
    Exit;
  Placement.Length := SizeOf(TWindowPlacement);
  GetWindowPlacement(Form.Handle, @Placement);
  with Placement, TForm(Form) do
  begin
    if not IsWindowVisible(Form.Handle) then
      ShowCmd := SW_HIDE;
    if LoadPosition then
    begin
      DataFound := False;
      Flags := IniReadInteger(IniFile, Section, DH_FLAGS, Flags);
      PosStr := ReadPosStr(IniFile, Section, DH_MIN_MAX_POS);
      if PosStr <> '' then
      begin
        DataFound := True;
        ptMinPosition.X := StrToIntDef(ExtractWord(1, PosStr, Delims), 0);
        ptMinPosition.Y := StrToIntDef(ExtractWord(2, PosStr, Delims), 0);
        ptMaxPosition.X := StrToIntDef(ExtractWord(3, PosStr, Delims), 0);
        ptMaxPosition.Y := StrToIntDef(ExtractWord(4, PosStr, Delims), 0);
      end;
      PosStr := ReadPosStr(IniFile, Section, DH_NORM_POS);
      if PosStr <> EmptyStr then
      begin
        DataFound := True;
        rcNormalPosition.Left := StrToIntDef(ExtractWord(1, PosStr, Delims), Left);
        rcNormalPosition.Top := StrToIntDef(ExtractWord(2, PosStr, Delims), Top);
        rcNormalPosition.Right := StrToIntDef(ExtractWord(3, PosStr, Delims), Left + Width);
        rcNormalPosition.Bottom := StrToIntDef(ExtractWord(4, PosStr, Delims), Top + Height);
      end;
      if Screen.PixelsPerInch <> IniReadInteger(IniFile, Section, DH_PIXELS, Screen.PixelsPerInch) then
        DataFound := False;
      if DataFound then
      begin
        if not (BorderStyle in [bsSizeable, bsSizeToolWin]) then
          rcNormalPosition := Rect(rcNormalPosition.Left, rcNormalPosition.Top, rcNormalPosition.Left + Width, rcNormalPosition.Top + Height);
        if rcNormalPosition.Right > rcNormalPosition.Left then
        begin
          if (Position in [poScreenCenter, poDesktopCenter]) and not (csDesigning in ComponentState) then
          begin
            TCnHackComponent(Form).SetDesigning(True);
            try
              Position := poDesigned;
            finally
              TCnHackComponent(Form).SetDesigning(False);
            end;
          end;
          SetWindowPlacement(Handle, @Placement);
        end;
      end;
    end;
    if LoadState then
    begin
      WinState := wsNormal; 
      { default maximize MDI main form }
      if ((Application.MainForm = Form) or (Application.MainForm = nil)) and ((FormStyle = fsMDIForm) or ((FormStyle = fsNormal) and (Position = poDefault))) then
        WinState := wsMaximized;
      ShowCmd := IniReadInteger(IniFile, Section, DH_SHOW_CMD, SW_HIDE);
      case ShowCmd of
        SW_SHOWNORMAL, SW_RESTORE, SW_SHOW:
          WinState := wsNormal;
        SW_MINIMIZE, SW_SHOWMINIMIZED, SW_SHOWMINNOACTIVE:
          WinState := wsMinimized;
        SW_MAXIMIZE:
          WinState := wsMaximized;
      end;
      if (WinState = wsMinimized) and ((Form = Application.MainForm) or (Application.MainForm = nil)) then
      begin
        TCnNastyForm(Form).FWindowState := wsNormal;
        PostMessage(Application.Handle, WM_SYSCOMMAND, SC_MINIMIZE, 0);
        Exit;
      end;
      if FormStyle in [fsMDIChild, fsMDIForm] then
        TCnNastyForm(Form).FWindowState := WinState
      else
        WindowState := WinState;
    end;
    Update;
  end;
end;

procedure ReadFormPlacementReg(Form: TForm; IniFile: TRegIniFile; const Section: string; LoadState, LoadPosition: Boolean);
begin
  InternalReadFormPlacement(Form, IniFile, Section, LoadState, LoadPosition);
end;

procedure ReadFormPlacement(Form: TForm; IniFile: TIniFile; const Section: string; LoadState, LoadPosition: Boolean);
begin
  InternalReadFormPlacement(Form, IniFile, Section, LoadState, LoadPosition);
end;

procedure RestoreFormPlacement(Form: TForm; const IniFileName: string; UseRegistry: Boolean);
var
  IniFile: TObject;
begin
  if UseRegistry then
  begin
    IniFile := TRegIniFile.Create(IniFileName);
    TRegIniFile(IniFile).Access := KEY_READ;
  end
  else
    IniFile := TIniFile.Create(IniFileName);
  try
    InternalReadFormPlacement(Form, IniFile, Form.ClassName, True, True);
  finally
    IniFile.Free;
  end;
end;

function GetUniqueFileNameInDir(const Path, FileNameMask: string): string;
var
  CurrentName: string;
  I: Integer;
begin
  Result := EmptyStr;
  for I := 0 to MaxInt do
  begin
    CurrentName := Format(FileNameMask, [I]);
    if not FileExists(NormalDir(Path) + CurrentName) then
    begin
      Result := CurrentName;
      Exit;
    end;
  end;
end;

procedure AppBroadcast(Msg, wParam: Longint; lParam: Longint);
var
  I: Integer;
begin
  for I := 0 to Screen.FormCount - 1 do
    SendMessage(Screen.Forms[I].Handle, Msg, wParam, lParam);
end;

procedure AppTaskbarIcons(AppOnly: Boolean);
var
  Style: Longint;
begin
  Style := GetWindowLong(Application.Handle, GWL_STYLE);
  if AppOnly then
    Style := Style or WS_CAPTION
  else
    Style := Style and not WS_CAPTION;
  SetWindowLong(Application.Handle, GWL_STYLE, Style);
  if AppOnly then
    SwitchToWindow(Application.Handle, False);
end; 

{$ENDIF SUPPORT_ADO}
end.
