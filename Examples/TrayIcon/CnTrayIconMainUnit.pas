unit CnTrayIconMainUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, CnClasses, CnTrayIcon, Menus, ShellAPI;

type
  TTrayIconDemoFrm = class(TForm)
    tiCnTrayIconDemo: TCnTrayIcon;
    pmCnTrayIconDemo: TPopupMenu;
    About1: TMenuItem;
    Exit1: TMenuItem;
    ShowApp1: TMenuItem;
    HideApp1: TMenuItem;
    BalloonHint1: TMenuItem;
    N1: TMenuItem;
    ShowIcon1: TMenuItem;
    HideIcon1: TMenuItem;
    procedure Exit1Click(Sender: TObject);
    procedure tiCnTrayIconDemoClick(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure tiCnTrayIconDemoDblClick(Sender: TObject);
    procedure BalloonHint1Click(Sender: TObject);
    procedure HideApp1Click(Sender: TObject);
    procedure ShowApp1Click(Sender: TObject);
    procedure HideIcon1Click(Sender: TObject);
    procedure ShowIcon1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  TrayIconDemoFrm: TTrayIconDemoFrm;

implementation

uses CnConsts;

{$R *.dfm}

procedure TTrayIconDemoFrm.Exit1Click(Sender: TObject);
begin
  if MessageDlg('Sure to quit CnTrayIcon Demo?', mtInformation, [mbYes, mbCancel], 0) = mrYes then
  Application.Terminate;
end;

procedure TTrayIconDemoFrm.tiCnTrayIconDemoClick(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if MessageDlg('Tray Icon Clicked!'+ #13+
  'Will you Visit CnPack Website?', mtInformation, [mbYes, mbCancel], 0) = mrYes then
    ShellExecute(Handle, 'Open', PChar(SCnPackUrl), nil, nil, SW_SHOWNORMAL);
end;

procedure TTrayIconDemoFrm.tiCnTrayIconDemoDblClick(Sender: TObject);
begin
  MessageDlg('Tray Icon Double Clicked!', mtInformation, [mbOK], 0)
end;

procedure TTrayIconDemoFrm.BalloonHint1Click(Sender: TObject);
begin
  tiCnTrayIconDemo.BalloonHint('hello', 'I am ballon hint demo of CnTrayIcon', btWarning, 10);
end;

procedure TTrayIconDemoFrm.HideApp1Click(Sender: TObject);
begin
  tiCnTrayIconDemo.HideApplication;
end;

procedure TTrayIconDemoFrm.ShowApp1Click(Sender: TObject);
begin
  tiCnTrayIconDemo.ShowApplication;
end;

procedure TTrayIconDemoFrm.HideIcon1Click(Sender: TObject);
begin
  tiCnTrayIconDemo.HideIcon;
end;

procedure TTrayIconDemoFrm.ShowIcon1Click(Sender: TObject);
begin
  tiCnTrayIconDemo.ShowIcon;
end;

end.
