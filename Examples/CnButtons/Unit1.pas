unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  CnButtons, StdCtrls, ExtCtrls;

type
  TForm1 = class(TForm)
    btn1: TCnButton;
    btn2: TCnButton;
    btn3: TCnButton;
    btn4: TCnButton;
    btn5: TCnButton;
    btn6: TCnButton;
    btn7: TCnButton;
    btn8: TCnButton;
    btn9: TCnButton;
    btn10: TCnButton;
    btn11: TCnButton;
    btn12: TCnButton;
    chk1: TCheckBox;
    chk2: TCheckBox;
    chk3: TCheckBox;
    pnl1: TPanel;
    btn13: TCnButton;
    btn14: TCnButton;
    bvl1: TBevel;
    btn15: TCnButton;
    btn16: TCnButton;
    bvl2: TBevel;
    btn17: TCnBitBtn;
    btn18: TCnBitBtn;
    btn19: TCnBitBtn;
    btn20: TCnBitBtn;
    btn21: TCnBitBtn;
    btn22: TCnBitBtn;
    btn23: TCnBitBtn;
    btn24: TCnBitBtn;
    bvl3: TBevel;
    btn25: TCnSpeedButton;
    bvl4: TBevel;
    btn26: TCnButton;
    btn27: TCnButton;
    btn28: TCnButton;
    btn29: TCnSpeedButton;
    btn30: TCnSpeedButton;
    btn31: TCnSpeedButton;
    btn32: TCnSpeedButton;
    btn33: TCnSpeedButton;
    btn34: TCnSpeedButton;
    btn35: TCnSpeedButton;
    btn36: TCnSpeedButton;
    btn37: TCnSpeedButton;
    btn38: TCnSpeedButton;
    btn39: TCnSpeedButton;
    btn40: TCnBitBtn;
    btn41: TCnBitBtn;
    btn42: TCnBitBtn;
    btn43: TCnBitBtn;
    btn44: TCnBitBtn;
    img1: TImage;
    procedure chk1Click(Sender: TObject);
    procedure chk2Click(Sender: TObject);
    procedure chk3Click(Sender: TObject);
    procedure CnBtnClick(Sender: TObject);
    procedure btn14Click(Sender: TObject);
    procedure SpeedBtnClick(Sender: TObject);
  private
    { Private declarations }
    procedure ChangeAllEnabled(Value: Boolean);
    procedure ChangeAllDownBold(Value: Boolean);
    procedure ChangeAllHotTrackBold(Value: Boolean);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses Unit2;

{$R *.DFM}

{ TForm1 }

procedure TForm1.ChangeAllDownBold(Value: Boolean);
var
  I: Integer;
begin
  for I := 0 to Self.ComponentCount - 1 do
    if Self.Components[I] is TCnButton then
      (Self.Components[I] as TCnButton).DownBold := Value
    else if Self.Components[I] is TCnBitBtn then
      (Self.Components[I] as TCnBitBtn).DownBold := Value
end;

procedure TForm1.ChangeAllEnabled(Value: Boolean);
var
  I: Integer;
begin
  for I := 0 to Self.ComponentCount - 1 do
    if Self.Components[I] is TCnButton then
      (Self.Components[I] as TCnButton).Enabled := Value
    else if Self.Components[I] is TCnBitBtn then
      (Self.Components[I] as TCnBitBtn).Enabled := Value
    else if Self.Components[I] is TCnSpeedButton then
      (Self.Components[I] as TCnSpeedButton).Enabled := Value
end;

procedure TForm1.ChangeAllHotTrackBold(Value: Boolean);
var
  I: Integer;
begin
  for I := 0 to Self.ComponentCount - 1 do
    if Self.Components[I] is TCnButton then
      (Self.Components[I] as TCnButton).HotTrackBold := Value
    else if Self.Components[I] is TCnBitBtn then
      (Self.Components[I] as TCnBitBtn).HotTrackBold := Value;
end;

procedure TForm1.chk1Click(Sender: TObject);
begin
  ChangeAllEnabled(chk1.Checked);
end;

procedure TForm1.chk2Click(Sender: TObject);
begin
  ChangeAllDownBold(chk2.Checked);
end;

procedure TForm1.chk3Click(Sender: TObject);
begin
  ChangeAllHotTrackBold(chk3.Checked);
end;

procedure TForm1.CnBtnClick(Sender: TObject);
begin
  if Sender is TCnButton then
    pnl1.Caption := (Sender as TCnButton).Name + ': "' +
      (Sender as TCnButton).Caption + '" Clicked!'
  else if Sender is TCnBitBtn then
    pnl1.Caption := (Sender as TCnBitBtn).Name + ': "' +
      (Sender as TCnBitBtn).Caption + '" Clicked!'
end;

procedure TForm1.btn14Click(Sender: TObject);
begin
  if Form2.ShowModal = mrOK then
    pnl1.Caption := Form2.btn1.Name + ': "' +
      Form2.btn1.Caption + '" Clicked!'
  else
    pnl1.Caption := Form2.btn2.Name + ': "' +
      Form2.btn2.Caption + '" Clicked!'
end;

procedure TForm1.SpeedBtnClick(Sender: TObject);
begin
  if Sender is TCnSpeedButton then
    pnl1.Caption := (Sender as TCnSpeedButton).Name + ': "' +
      (Sender as TCnSpeedButton).Caption + '" Clicked!'
end;

end.
