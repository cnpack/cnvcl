unit Main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, Menus, ComCtrls, ToolWin, CnDockFormControl, CnDockTree,
  CnVCDockStyle, CnDelphiDockStyle, CnVIDDockStyle, CnVSNETDockStyle, CnClasses,
  CnDockSupportClass, IniFiles{$IFDEF VER150}, XPMan{$ENDIF};

type
  TMainForm = class(TForm)
    MainMenu1: TMainMenu;
    DockForm_Menu: TMenuItem;
    DelphiStyle: TMenuItem;
    VCStyle: TMenuItem;
    VIDStyle: TMenuItem;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ShowWindow_Menu: TMenuItem;
    PopupMenu1: TPopupMenu;
    N5: TMenuItem;
    N6: TMenuItem;
    N7: TMenuItem;
    N8: TMenuItem;
    Set_Menu: TMenuItem;
    N10: TMenuItem;
    N11: TMenuItem;
    N12: TMenuItem;
    N13: TMenuItem;
    N14: TMenuItem;
    N15: TMenuItem;
    N16: TMenuItem;
    N17: TMenuItem;
    bsToolWindow1: TMenuItem;
    N18: TMenuItem;
    bsDialog1: TMenuItem;
    bsNone1: TMenuItem;
    bsSingle1: TMenuItem;
    bsSizeable1: TMenuItem;
    bsSizeToolWin1: TMenuItem;
    bsToolWindow2: TMenuItem;
    DockInfo_Menu: TMenuItem;
    SaveToFile: TMenuItem;
    LoadFromFile: TMenuItem;
    SaveToReg: TMenuItem;
    LoadFromReg: TMenuItem;
    N24: TMenuItem;
    DockOption_Menu: TMenuItem;
    TopDocked: TMenuItem;
    BottomDocked: TMenuItem;
    LeftDocked: TMenuItem;
    RightDocked: TMenuItem;
    AllDocked: TMenuItem;
    N31: TMenuItem;
    CnDockServer1: TCnDockServer;
    CnDelphiDockStyle1: TCnDelphiDockStyle;
    CnVCDockStyle1: TCnVCDockStyle;
    CnVIDDockStyle1: TCnVIDDockStyle;
    StatusBar1: TStatusBar;
    DockStyle_Menu: TMenuItem;
    DelphiDockStyle: TMenuItem;
    VCDockStyle: TMenuItem;
    VIDDockStyle: TMenuItem;
    Default: TMenuItem;
    ToolButton4: TToolButton;
    VSNETStyle: TMenuItem;
    PopupMenu2: TPopupMenu;
    ClientDockorFloat: TMenuItem;
    ClientHide: TMenuItem;
    ClientTopDocked: TMenuItem;
    ClientBottomDocked: TMenuItem;
    ClientLeftDocked: TMenuItem;
    ClientRightDocked: TMenuItem;
    N20: TMenuItem;
    N21: TMenuItem;
    ClientEachOtherDocked: TMenuItem;
    ClientAllDocked: TMenuItem;
    Memo1: TMemo;
    CnVSNETDockStyle1: TCnVSNETDockStyle;
    procedure DelphiStyleClick(Sender: TObject);
    procedure VCStyleClick(Sender: TObject);
    procedure VIDStyleClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure N5Click(Sender: TObject);
    procedure N10Click(Sender: TObject);
    procedure N11Click(Sender: TObject);
    procedure bsToolWindow1Click(Sender: TObject);
    procedure bsToolWindow2Click(Sender: TObject);
    procedure SaveToFileClick(Sender: TObject);
    procedure LoadFromFileClick(Sender: TObject);
    procedure SaveToRegClick(Sender: TObject);
    procedure LoadFromRegClick(Sender: TObject);
    procedure TopDockedClick(Sender: TObject);
    procedure BottomDockedClick(Sender: TObject);
    procedure LeftDockedClick(Sender: TObject);
    procedure RightDockedClick(Sender: TObject);
    procedure AllDockedClick(Sender: TObject);
    procedure DefaultClick(Sender: TObject);
    procedure DelphiDockStyleClick(Sender: TObject);
    procedure VCDockStyleClick(Sender: TObject);
    procedure VIDDockStyleClick(Sender: TObject);
    procedure DockForm4Click(Sender: TObject);
    procedure PopupMenu2Popup(Sender: TObject);
    procedure ClientTopDockedClick(Sender: TObject);
    procedure ClientBottomDockedClick(Sender: TObject);
    procedure ClientLeftDockedClick(Sender: TObject);
    procedure ClientRightDockedClick(Sender: TObject);
    procedure ClientEachOtherDockedClick(Sender: TObject);
    procedure ClientAllDockedClick(Sender: TObject);
    procedure ClientDockorFloatClick(Sender: TObject);
    procedure ClientHideClick(Sender: TObject);
  private
    { Private declarations }
    FForm1Count,
    FForm2Count,
    FForm3Count,
    FForm4Count: Integer;
    procedure AddItemToShowDockMenu(AForm: TForm);
    procedure ShowDockWindowMenuClick(Sender: TObject);
  public
    { Public declarations }
  end;

const
  BoolStr: array[Boolean] of string =
    ('FALSE', 'TRUE');

var
  MainForm: TMainForm;

implementation

uses Unit1, Unit2, Unit3, Unit4, CnDockSupportProc;

{$R *.DFM}

procedure TMainForm.DelphiStyleClick(Sender: TObject);
var Form: TForm1;
begin
  Form := TForm1.Create(Application);
  Form.Caption := Form.Caption + ' _ ' + IntToStr(FForm1Count);
  Inc(FForm1Count);
  AddItemToShowDockMenu(Form);
end;

procedure TMainForm.VCStyleClick(Sender: TObject);
var Form: TForm2;
begin
  Form := TForm2.Create(Application);
  Form.Caption := Form.Caption + ' _ ' + IntToStr(FForm2Count);
  Inc(FForm2Count);
  AddItemToShowDockMenu(Form);
end;

procedure TMainForm.VIDStyleClick(Sender: TObject);
var Form: TForm3;
begin
  Form := TForm3.Create(Self);
  Form.Caption := Form.Caption + ' _ ' + IntToStr(FForm3Count);
  if FForm3Count > 0 then
    Form.CnDockClient1.DockLevel := 1;
//    Form.CnDockClient1.LeftDock := False;
  Inc(FForm3Count);
  AddItemToShowDockMenu(Form);
end;

procedure TMainForm.DockForm4Click(Sender: TObject);
var Form: TForm4;
begin
  Form := TForm4.Create(Self);
  Form.Caption := Form.Caption + ' _ ' + IntToStr(FForm4Count);
  Inc(FForm4Count);
  AddItemToShowDockMenu(Form);
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FForm1Count := 0;
  FForm2Count := 0;
  FForm3Count := 0;
  FForm4Count := 0;
  TopDocked.Checked := CnDockServer1.TopDock;
  BottomDocked.Checked := CnDockServer1.BottomDock;
  LeftDocked.Checked := CnDockServer1.LeftDock;
  RightDocked.Checked := CnDockServer1.RightDock;
  AllDocked.Checked := CnDockServer1.EnableDock;
  Memo1.WordWrap := True;
end;

procedure TMainForm.AddItemToShowDockMenu(AForm: TForm);
var
  AMenuItem: TMenuItem;
begin
  AMenuItem := NewItem(AForm.Caption, 0, True, True,
    ShowDockWindowMenuClick, 0, '');
  ShowWindow_Menu.Add(AMenuItem);
  AMenuItem.Tag := Integer(AForm);
  AForm.Tag := Integer(AMenuItem);
end;

procedure TMainForm.ShowDockWindowMenuClick(Sender: TObject);
var MenuItem: TMenuItem;
  Form: TForm;
begin
  MenuItem := TMenuItem(Sender);
  Form := TForm(MenuItem.Tag);
  if MenuItem.Checked then
  begin
    if GetFormVisible(Form) then
    begin
      HideDockForm(Form);
      MenuItem.Checked := False;
    end else
      ShowDockForm(Form);
  end else
  begin
    ShowDockForm(Form);
    MenuItem.Checked := True;
  end;
end;

procedure TMainForm.N5Click(Sender: TObject);
var DockPage: TCnTabPageControl;
begin
  if PopupMenu1.PopupComponent is TCnTabPageControl then
  begin
    DockPage := PopupMenu1.PopupComponent as TCnTabPageControl;
    case TPopupMenu(Sender).Tag of
      1: DockPage.TabPosition := tpTop;
      2: DockPage.TabPosition := tpLeft;
      3: DockPage.TabPosition := tpBottom;
      4: DockPage.TabPosition := tpRight;
    end;
  end;
end;

procedure TMainForm.N10Click(Sender: TObject);
begin
  N10.Checked := not N10.Checked;
  if N10.Checked then
    SetDockPageControlPopupMenu(PopupMenu1)
  else SetDockPageControlPopupMenu(nil);
end;

procedure TMainForm.N11Click(Sender: TObject);
begin
  N11.Checked := not N11.Checked;
  if N11.Checked then
    SetDockPageControlHotTrack(True)
  else SetDockPageControlHotTrack(False);
end;

procedure TMainForm.bsToolWindow1Click(Sender: TObject);
begin
  case TMenuItem(Sender).Tag of
    1: SetTabDockHostBorderStyle(bsDialog);
    2: SetTabDockHostBorderStyle(bsNone);
    3: SetTabDockHostBorderStyle(bsSingle);
    4: SetTabDockHostBorderStyle(bsSizeable);
    5: SetTabDockHostBorderStyle(bsSizeToolWin);
    6: SetTabDockHostBorderStyle(bsToolWindow);
  end;
end;

procedure TMainForm.bsToolWindow2Click(Sender: TObject);
begin
  case TMenuItem(Sender).Tag of
    1: SetConjoinDockHostBorderStyle(bsDialog);
    2: SetConjoinDockHostBorderStyle(bsNone);
    3: SetConjoinDockHostBorderStyle(bsSingle);
    4: SetConjoinDockHostBorderStyle(bsSizeable);
    5: SetConjoinDockHostBorderStyle(bsSizeToolWin);
    6: SetConjoinDockHostBorderStyle(bsToolWindow);
  end;
end;

procedure TMainForm.SaveToFileClick(Sender: TObject);
begin
  SaveDockTreeToFile(ExtractFilePath(Application.ExeName) + 'DockInfo.ini');
end;

procedure TMainForm.LoadFromFileClick(Sender: TObject);
begin
  LoadDockTreeFromFile(ExtractFilePath(Application.ExeName) + 'DockInfo.ini');
end;

procedure TMainForm.SaveToRegClick(Sender: TObject);
begin
  SaveDockTreeToReg(HKEY_CURRENT_USER, '\Software\DockInfo');
end;

procedure TMainForm.LoadFromRegClick(Sender: TObject);
begin
  LoadDockTreeFromReg(HKEY_CURRENT_USER, '\Software\DockInfo');
end;

procedure TMainForm.TopDockedClick(Sender: TObject);
begin
  TopDocked.Checked := not TopDocked.Checked;
  CnDockServer1.TopDock := TopDocked.Checked;
end;

procedure TMainForm.BottomDockedClick(Sender: TObject);
begin
  BottomDocked.Checked := not BottomDocked.Checked;
  CnDockServer1.BottomDock := BottomDocked.Checked;
end;

procedure TMainForm.LeftDockedClick(Sender: TObject);
begin
  LeftDocked.Checked := not LeftDocked.Checked;
  CnDockServer1.LeftDock := LeftDocked.Checked;
end;

procedure TMainForm.RightDockedClick(Sender: TObject);
begin
  RightDocked.Checked := not RightDocked.Checked;
  CnDockServer1.RightDock := RightDocked.Checked;
end;

procedure TMainForm.AllDockedClick(Sender: TObject);
begin
  AllDocked.Checked := not AllDocked.Checked;
  CnDockServer1.EnableDock := AllDocked.Checked;
end;

procedure TMainForm.DefaultClick(Sender: TObject);
begin
  CnDockServer1.DockStyle := nil;
end;

procedure TMainForm.DelphiDockStyleClick(Sender: TObject);
begin
  CnDockServer1.DockStyle := CnDelphiDockStyle1;
end;

procedure TMainForm.VCDockStyleClick(Sender: TObject);
begin
  CnDockServer1.DockStyle := CnVCDockStyle1;
end;

procedure TMainForm.VIDDockStyleClick(Sender: TObject);
begin
  CnDockServer1.DockStyle := CnVIDDockStyle1;
end;


procedure TMainForm.PopupMenu2Popup(Sender: TObject);
var DockClient: TCnDockClient;
begin
  if PopupMenu2.PopupComponent is TForm then
  begin
    DockClient := FindDockClient(TForm(PopupMenu2.PopupComponent));
    if DockClient <> nil then
    begin
      ClientTopDocked.Checked := DockClient.TopDock;
      ClientBottomDocked.Checked := DockClient.BottomDock;
      ClientLeftDocked.Checked := DockClient.LeftDock;
      ClientRightDocked.Checked := DockClient.RightDock;
      ClientEachOtherDocked.Checked := DockClient.EachOtherDock;
      ClientAllDocked.Checked := DockClient.EnableDock;
      if DockClient.DockState = DS_Floating then
        ClientDockorFloat.Caption := 'Í£¿¿'
      else ClientDockorFloat.Caption := '¸¡¶¯';
    end;
  end;
end;

procedure TMainForm.ClientTopDockedClick(Sender: TObject);
var DockClient: TCnDockClient;
begin
  if PopupMenu2.PopupComponent is TForm then
  begin
    DockClient := FindDockClient(TForm(PopupMenu2.PopupComponent));
    if DockClient <> nil then
    begin
      ClientTopDocked.Checked := not ClientTopDocked.Checked;
      DockClient.TopDock := ClientTopDocked.Checked;
    end;
  end;
end;

procedure TMainForm.ClientBottomDockedClick(Sender: TObject);
var DockClient: TCnDockClient;
begin
  if PopupMenu2.PopupComponent is TForm then
  begin
    DockClient := FindDockClient(TForm(PopupMenu2.PopupComponent));
    if DockClient <> nil then
    begin
      ClientBottomDocked.Checked := not ClientBottomDocked.Checked;
      DockClient.BottomDock := ClientBottomDocked.Checked;
    end;
  end;
end;

procedure TMainForm.ClientLeftDockedClick(Sender: TObject);
var DockClient: TCnDockClient;
begin
  if PopupMenu2.PopupComponent is TForm then
  begin
    DockClient := FindDockClient(TForm(PopupMenu2.PopupComponent));
    if DockClient <> nil then
    begin
      ClientLeftDocked.Checked := not ClientLeftDocked.Checked;
      DockClient.LeftDock := ClientLeftDocked.Checked;
    end;
  end;
end;

procedure TMainForm.ClientRightDockedClick(Sender: TObject);
var DockClient: TCnDockClient;
begin
  if PopupMenu2.PopupComponent is TForm then
  begin
    DockClient := FindDockClient(TForm(PopupMenu2.PopupComponent));
    if DockClient <> nil then
    begin
      ClientRightDocked.Checked := not ClientRightDocked.Checked;
      DockClient.RightDock := ClientRightDocked.Checked;
    end;
  end;
end;

procedure TMainForm.ClientEachOtherDockedClick(Sender: TObject);
var DockClient: TCnDockClient;
begin
  if PopupMenu2.PopupComponent is TForm then
  begin
    DockClient := FindDockClient(TForm(PopupMenu2.PopupComponent));
    if DockClient <> nil then
    begin
      ClientEachOtherDocked.Checked := not ClientEachOtherDocked.Checked;
      DockClient.EachOtherDock := ClientEachOtherDocked.Checked;
    end;
  end;
end;

procedure TMainForm.ClientAllDockedClick(Sender: TObject);
var DockClient: TCnDockClient;
begin
  if PopupMenu2.PopupComponent is TForm then
  begin
    DockClient := FindDockClient(TForm(PopupMenu2.PopupComponent));
    if DockClient <> nil then
    begin
      ClientAllDocked.Checked := not ClientAllDocked.Checked;
      DockClient.EnableDock := ClientAllDocked.Checked;
    end;
  end;
end;

procedure TMainForm.ClientDockorFloatClick(Sender: TObject);
var DockClient: TCnDockClient;
begin
  if PopupMenu2.PopupComponent is TForm then
  begin
    DockClient := FindDockClient(TForm(PopupMenu2.PopupComponent));
    if DockClient <> nil then
      DockClient.RestoreChild;
  end;
end;

procedure TMainForm.ClientHideClick(Sender: TObject);
var DockClient: TCnDockClient;
begin
  if PopupMenu2.PopupComponent is TForm then
  begin
    DockClient := FindDockClient(TForm(PopupMenu2.PopupComponent));
    if DockClient <> nil then
      DockClient.HideParentForm;
  end;
end;

end.
