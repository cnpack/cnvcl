unit Unit1;

interface

uses
  CnVolumeCtrl, CnCommon,
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, CnClasses;

type
  TForm1 = class(TForm)
    mmo1: TMemo;
    btn1: TButton;
    cbb1: TComboBox;
    cbb2: TComboBox;
    lbl1: TLabel;
    lbl2: TLabel;
    TrackBar1: TTrackBar;
    TrackBar2: TTrackBar;
    chk1: TCheckBox;
    btn2: TButton;
    CnVolumeCtrl1: TCnVolumeCtrl;
    procedure FormCreate(Sender: TObject);
    procedure btn1Click(Sender: TObject);
    procedure cbb1Change(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure TrackBar2Change(Sender: TObject);
    procedure chk1Click(Sender: TObject);
    procedure btn2Click(Sender: TObject);
    procedure CnVolumeCtrl1MuteChange(bMute: Boolean);
    procedure CnVolumeCtrl1VolumeChange(Volume: TCnVolume;
      Balance: TCnBalance);
  private
    procedure LoadDevs;
    procedure LoadLines;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  LoadDevs;
  LoadLines;
end;

procedure TForm1.btn1Click(Sender: TObject);
var
  i, j: Integer;
begin
  with mmo1.Lines, CnVolumeCtrl1 do
  begin
    Add(Format('Devs: %d', [Devs]));
    for j := 0 to Devs - 1 do
    begin
      Add(Format('[%d] Device: %s    Lines: %d', [j, GetDevCaption(j), GetDevLines(j)]));
      for i := 0 to GetDevLines(j) - 1 do
      begin
        Add(Format('    [%d] Line: %s', [i, GetLineCaption(j, i)]));
        Add(Format('        Volume  [%d]', [GetLineVolume(j, i)]));
        Add(Format('        Balance [%s] [%d]',
          [BoolToStr(GetLineHaveBalance(j, i), True), GetLineBalance(j, i)]));
        Add(Format('        Mute    [%s]', [BoolToStr(GetLineMute(j, i), True)]));
      end;
    end;
  end;
end;

procedure TForm1.LoadDevs;
var
  i: Integer;
begin
  with CnVolumeCtrl1, cbb1 do
  begin
    Clear;
    for i := 0 to Devs - 1 do
      Items.Add(GetDevCaption(i));
    ItemIndex := 0;
  end;
end;

procedure TForm1.LoadLines;
var
  i, j: Integer;
begin
  j := cbb1.ItemIndex;
  with CnVolumeCtrl1, cbb2 do
  begin
    Clear;
    for i := 0 to Lines - 1 do
      Items.Add(GetLineCaption(j, i));
    ItemIndex := 0;
  end;

  TrackBar1.Position := CnVolumeCtrl1.Balance;
  TrackBar2.Position := 255 - CnVolumeCtrl1.Volume;
  chk1.Checked := CnVolumeCtrl1.IsMute;
end;

procedure TForm1.cbb1Change(Sender: TObject);
begin
  case TComboBox(Sender).Tag of
    0: //Device
      begin
        CnVolumeCtrl1.CurDev := TComboBox(Sender).ItemIndex;
        LoadLines;
      end;
    1: //Line
      begin
        CnVolumeCtrl1.CurLine := TComboBox(Sender).ItemIndex;
      end;
  end;

  TrackBar1.Position := CnVolumeCtrl1.Balance;
  TrackBar2.Position := 255 - CnVolumeCtrl1.Volume;
  chk1.Checked := CnVolumeCtrl1.IsMute;
end;

procedure TForm1.TrackBar1Change(Sender: TObject);
begin
  CnVolumeCtrl1.Balance := TrackBar1.Position;
end;

procedure TForm1.TrackBar2Change(Sender: TObject);
begin
  CnVolumeCtrl1.Volume := 255 - TrackBar2.Position;
end;

procedure TForm1.chk1Click(Sender: TObject);
begin
  CnVolumeCtrl1.IsMute := chk1.Checked;
end;

procedure TForm1.btn2Click(Sender: TObject);
var
  strAbout: string;
begin
  strAbout := '程序名称: 系统音量 控制器示例程序' + #13#10 +
              '程序作者: 小冬(kendling)(kendling@21cn.com)' + #13#10 +
              '程序说明: 使用TCnVolumeCtrl控件的示例' + #13#10 + #13#10 +
              '下载网站: http://CnPack.org' + #13#10 +
              '技术支持: master@CnPack.org' + #13#10 + #13#10 +
              '(C)Copyright 2001-2011 CnPack 开发组';
  ShowMessage(strAbout);
end;

procedure TForm1.CnVolumeCtrl1MuteChange(bMute: Boolean);
begin
  chk1.Checked := bMute;
  mmo1.Lines.Add(BoolToStr(bMute, True));
end;

procedure TForm1.CnVolumeCtrl1VolumeChange(Volume: TCnVolume;
  Balance: TCnBalance);
begin
  TrackBar1.Position := Balance;
  TrackBar2.Position := 255 - Volume;
  mmo1.Lines.Add(Format('Volume: %d Balance: %d', [Volume, Balance]));
end;

end.
