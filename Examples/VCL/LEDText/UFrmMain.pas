unit UFrmMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Mask, ComCtrls, 
  CnAOTreeView, CnLED, CnAutoOption, Spin;

type
  TFrmMain = class(TForm)
    RzPanel1: TPanel;
    RzLabel4: TLabel;
    RzLabel5: TLabel;
    cbbFont: TComboBox;
    seHeight: TSpinEdit;
    pnlLED: TPanel;
    RzStatusBar1: TStatusBar;
    btn3: TButton;
    btn2: TButton;
    pnl1: TPanel;
    btn1: TButton;
    mmo1: TMemo;
    RzSizePanel2: TPanel;
    mmoInfo: TMemo;
    atv1: TCnAOTreeView;
    spl1: TSplitter;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure seHeightChange(Sender: TObject);
    procedure cbbFontChange(Sender: TObject);
    procedure mmo1Change(Sender: TObject);
    procedure atv1Change(Sender: TObject; Node: TTreeNode);
    procedure btn3Click(Sender: TObject);
    procedure btn2Click(Sender: TObject);
    procedure btn1Click(Sender: TObject);
  private
    { Private declarations }
    sLed, FLEDCC: TCnLEDText;
    Option: TCnOptionGroup;
    procedure PackLed;
  public
    { Public declarations }
  end;

var
  FrmMain: TFrmMain;

implementation


{$R *.dfm}

procedure TFrmMain.atv1Change(Sender: TObject; Node: TTreeNode);
begin
  atv1.ApplyOption;
end;

procedure TFrmMain.btn1Click(Sender: TObject);
var
  s: string;
  i, j,inx: Integer;
  fs: TFileStream;
begin
  PackLed;
  i := 94 * 94;
  SetLength(s, i*2);
  inx := 1;
   for I := 1 to 94 do
      for j := 1 to 94 do
      begin
        s[inx] := Char(I + $A0);
        s[inx+1] := Char(J + $A0);
        Inc(inx,2);
      end;
  fs := TFileStream.Create('cc.zk', fmCreate);
  try
    FLEDCC.ExportWordInfo(s,fs);
    ShowMessage('字库信息已保存在cc.zk');
  finally
    fs.Free;
  end; 
end;

procedure TFrmMain.PackLed;
begin
  FLEDCC.ModeRight := sLed.ModeRight;
  FLEDCC.ModeBottom := sled.ModeBottom;
  FLEDCC.ModeColumn := sLed.ModeColumn;
  FLEDCC.FirstLowBit := sled.FirstLowBit;
end;

procedure TFrmMain.btn2Click(Sender: TObject);
var
  s: string;
  i, j,inx: Integer;
  fs: TFileStream;
begin
  PackLed;
  i := (87-16+1) * 94;
  SetLength(s, i*2);
  inx := 1;
   for I := 16 to 87 do
      for j := 1 to 94 do
      begin
        s[inx] := Char(I + $A0);
        s[inx+1] := Char(J + $A0);
        Inc(inx,2);
      end;
  fs := TFileStream.Create('cc.zk', fmCreate);
  try
    FLEDCC.ExportWordInfo(s,fs);
    ShowMessage('字库信息已保存在cc.zk');
  finally
    fs.Free;
  end; 
end;

procedure TFrmMain.btn3Click(Sender: TObject);
var
  str: string;
  fs: TFileStream;
begin
  if not InputQuery('输入', '输入要取点阵的汉字', str) then
    Exit;
  PackLed;
  fs := TFileStream.Create('cc.zk', fmCreate);
  try
    FLEDCC.ExportWordInfo(str,fs);
    ShowMessage('字库信息已保存在cc.zk');
  finally
    fs.Free;
  end;
  if FLEDCC.ModeStructOut = 0 then
  begin
    mmoInfo.Lines.LoadFromFile('cc.zk');
    mmoInfo.SelStart := 1;
  end;
end;

procedure TFrmMain.FormCreate(Sender: TObject);
begin
  cbbFont.Items.Assign(Screen.Fonts);
  sLed := TCnLEDText.Create(Self);
  sLed.Parent := pnl1;
  sLed.Align := alClient;
  sLed.Text := '汉';
  sLed.Animate:= True;

  FLEDCC := TCnLEDText.Create(self);
  FLEDCC.Parent := pnlLED;
  FLEDCC.Align := alClient;

  seHeight.Value := FLEDCC.Font.Height;
  cbbFont.Text := FLEDCC.Font.Name;

  Option := TCnOptionGroup.Create(nil);
  Option.Text := '参数设置';
  with Option.AddGroup('颜色') do
  begin
    AddItem(FLEDCC,'CellColor','点颜色');
    AddItem(FLEDCC,'CellBorderColor','点边框颜色');
    AddItem(FLEDCC,'CellHotColor','点高亮颜色');
    AddItem(FLEDCC,'WordBorderColor','字边框颜色');
    AddItem(sLed,'CellAnimateColor','动画字颜色');
  end;

  with Option.AddGroup('其它') do
  begin
    AddItem(FLEDCC,'PointSize','LED点高度');
    AddItem(FLEDCC,'CellBorderWidth','点边框线宽度');
    AddItem(FLEDCC,'WordBorderWidth','字边框线宽度');
  end;
  with Option.AddGroup('取点阵选项') do
  begin
    AddItem(sLed, 'ModeRight','左右','从左向右'#13#10'从右向左');
    AddItem(sLed, 'ModeBottom','上下','从上向下'#13#10'从下向上');
    AddItem(sLed, 'ModeColumn','行列','行取模方式'#13#10'列取模方式');
    AddItem(sLed, 'FirstLowBit','点序','高位在前'#13#10'低位在前');
    AddItem(FLEDCC, 'ModeStructOut','输出','C结构化输出'#13#10'字库格式输出');
  end;
  atv1.Options := Option;
end;

procedure TFrmMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(Option);
  FreeAndNil(FLEDCC);
  FreeAndNil(sLed);
end;

procedure TFrmMain.mmo1Change(Sender: TObject);
begin
  FLEDCC.ShowText(mmo1.Text);
end;

procedure TFrmMain.cbbFontChange(Sender: TObject);
begin
  FLEDCC.Font.Name := cbbFont.Text;
end;

procedure TFrmMain.seHeightChange(Sender: TObject);
begin
  FLEDCC.Font.Height := seHeight.Value;
end;

end.
