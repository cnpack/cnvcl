unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, CheckLst, ComCtrls, CnTaskBar, ExtCtrls;

type
  TForm1 = class(TForm)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    Image2: TImage;
    CheckListBox1: TCheckListBox;
    CheckListBox2: TCheckListBox;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    RadioButton2: TRadioButton;
    RadioButton1: TRadioButton;
    RadioButton3: TRadioButton;
    RadioButton4: TRadioButton;
    Button0: TButton;
    Button4: TButton;
    procedure RadioButton2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CheckListBox1ClickCheck(Sender: TObject);
    procedure CheckListBox1Click(Sender: TObject);
    procedure CheckListBox1DblClick(Sender: TObject);
    procedure RadioButton3Click(Sender: TObject);
    procedure CheckListBox2ClickCheck(Sender: TObject);
    procedure CheckListBox2DblClick(Sender: TObject);
    procedure Button0Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
  private
    { Private declarations }
    CnsysToolBar: TCnTaskBar;
  public
    { Public declarations }
    procedure ReSetList(ShowHideBtn: boolean=false);
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

{ TForm1 }

procedure TForm1.ReSetList(ShowHideBtn: boolean);
var
  i: integer;
begin
  CnsysToolBar.ShowHideBtn := ShowHideBtn;
  CheckListBox1.Items := CnsysToolBar.TrayBtnList;
  CheckListBox2.Items := CnsysToolBar.TaskBtnList;
  for i := 0 to CheckListBox1.Items.Count - 1 do
    CheckListBox1.Checked[i] := not TCnSysToolBarBtn(CnsysToolBar.TrayBtnList.Objects[i]).isSysHide;
  for i := 0 to CheckListBox2.Items.Count - 1 do
    CheckListBox2.Checked[i] := not CnsysToolBar.TaskBtns[i].isSysHide
end;

procedure TForm1.RadioButton2Click(Sender: TObject);
begin
  ReSetList(not RadioButton2.Checked);
  RadioButton3.Checked := RadioButton2.Checked;
  RadioButton4.Checked := RadioButton1.Checked;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  CnsysToolBar := TCnTaskBar.Create(Self);
  RadioButton2.Checked := true;
  RadioButton3.Checked := true;
end;

procedure TForm1.CheckListBox1ClickCheck(Sender: TObject);
begin
  if CheckListBox1.Checked[CheckListBox1.ItemIndex] then
    TCnSysToolBarBtn(CnsysToolBar.TrayBtnList.Objects[CheckListBox1.ItemIndex]).Visible := true
  else
    TCnSysToolBarBtn(CnsysToolBar.TrayBtnList.Objects[CheckListBox1.ItemIndex]).Visible := false;
  CnsysToolBar.HideTrayBtnClick;
end;

procedure TForm1.CheckListBox1Click(Sender: TObject);
begin
  if CheckListBox1.ItemIndex <> -1 then
  begin
    Image2.Width := CnsysToolBar.TrayBtns[CheckListBox1.ItemIndex].Picture.Width;
    Image2.Height := CnsysToolBar.TrayBtns[CheckListBox1.ItemIndex].Picture.Height;
    Image2.Picture.Assign(CnsysToolBar.TrayBtns[CheckListBox1.ItemIndex].Picture);
  end;
end;

procedure TForm1.CheckListBox1DblClick(Sender: TObject);
begin
  if CheckListBox1.ItemIndex <> -1 then
      CnsysToolBar.TrayBtns[CheckListBox1.ItemIndex].Click;
end;

procedure TForm1.RadioButton3Click(Sender: TObject);
begin
  ReSetList(not RadioButton3.Checked);
  RadioButton2.Checked := RadioButton3.Checked;
  RadioButton1.Checked := RadioButton4.Checked;
end;

procedure TForm1.CheckListBox2ClickCheck(Sender: TObject);
begin
  if CheckListBox2.Checked[CheckListBox2.ItemIndex] then
    TCnSysToolBarBtn(CnsysToolBar.TaskBtns[CheckListBox2.ItemIndex]).Visible := true
  else
    TCnSysToolBarBtn(CnsysToolBar.TaskBtns[CheckListBox2.ItemIndex]).Visible := false;
  CnsysToolBar.HideTrayBtnClick;
end;

procedure TForm1.CheckListBox2DblClick(Sender: TObject);
begin
  if CheckListBox2.ItemIndex <> -1 then
  begin
     CnsysToolBar.TaskBtns[CheckListBox2.ItemIndex].Click;
  end;
end;

procedure TForm1.Button0Click(Sender: TObject);
begin
  CnsysToolBar.HideOn;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  CnsysToolBar.StartBtnCaption := InputBox('请输入开始按扭信息','请输入开始按扭信息',CnsysToolBar.StartBtnCaption);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  if Button2.Caption = '隐藏开始按扭' then
  begin
    CnsysToolBar.StartBtnVisible := False;
    Button2.Caption := '显示开始按扭'
  end
  else
  begin
    CnsysToolBar.StartBtnVisible := True;
    Button2.Caption := '隐藏开始按扭'
  end;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  if Button3.Caption = '使开始按扭不可用' then
  begin
    CnsysToolBar.StartBtnEnabled := False;
    Button3.Caption := '使开始按扭可用'
  end
  else
  begin
    CnsysToolBar.StartBtnEnabled := True;
    Button3.Caption := '使开始按扭不可用'
  end;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  if Button4.Caption = '隐藏任务栏' then
  begin
    CnsysToolBar.Visible := False;
    Button4.Caption := '显示任务栏'
  end
  else
  begin
    CnsysToolBar.Visible := True;
    Button4.Caption := '隐藏任务栏'
  end;
end;

end.

