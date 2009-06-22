unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, CnFMForm, StdCtrls, ExtCtrls, WinSkinData, Menus;

type
  TForm1 = class(TForm)
    CnFMForm1: TCnFMForm;
    CnFMGlassPanel1: TCnFMGlassPanel;
    CnFMTitleImg1: TCnFMTitleImg;
    CnFMMenuLabel1: TCnFMMenuLabel;
    Panel1: TPanel;
    PopupMenu1: TPopupMenu;
    New1: TMenuItem;
    Open1: TMenuItem;
    Save1: TMenuItem;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    SkinData1: TSkinData;
    procedure FormCreate(Sender: TObject);
    procedure CnFMForm1CloseButtonClick(Sender: TObject);
    procedure CnFMForm1MinButtonClick(Sender: TObject);
    procedure CnFMForm1MaxButtonClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  CnFMForm1.LoadSkin('Skins\Yellow');
  //RaNGlassPanel1.BringToFront;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  CnFMForm1.ResetSkin;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  CnFMForm1.MaxButtonVisible := True;
  CnFMForm1.SetButtonPosition;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  CnFMForm1.MaxButtonVisible := False;
  CnFMForm1.SetButtonPosition;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  CnFMForm1.SetSysButtonEnable(True, True, True);
end;

procedure TForm1.CnFMForm1CloseButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TForm1.CnFMForm1MaxButtonClick(Sender: TObject);
begin
  if self.WindowState = wsNormal then
    self.WindowState := wsMaximized
  else if self.WindowState = wsMaximized then
    Self.WindowState := wsNormal;
end;

procedure TForm1.CnFMForm1MinButtonClick(Sender: TObject);
begin
  Application.Minimize;
end;

end.
