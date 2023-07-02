unit UnitQuantum;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, CnQuantum, StdCtrls;

type
  TFormQuantum = class(TForm)
    pbQ: TPaintBox;
    lblSetTheta: TLabel;
    edtSetTheta: TEdit;
    edtSetPhi: TEdit;
    lblSetPhi: TLabel;
    btnSetTheta: TButton;
    btnSetPhi: TButton;
    lblQ: TLabel;
    btnHadamard: TButton;
    lblOutQ: TLabel;
    btnPauliX: TButton;
    btnPauliY: TButton;
    btnPauliZ: TButton;
    lblNewTheta: TLabel;
    edtNewTheta: TEdit;
    btnPhaseShift: TButton;
    procedure pbQPaint(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnSetThetaClick(Sender: TObject);
    procedure btnSetPhiClick(Sender: TObject);
    procedure btnHadamardClick(Sender: TObject);
    procedure btnPauliXClick(Sender: TObject);
    procedure btnPauliYClick(Sender: TObject);
    procedure btnPauliZClick(Sender: TObject);
    procedure btnPhaseShiftClick(Sender: TObject);
  private
    FQ: TCnQuBit;
    FOutQ: TCnQuBit;
  public
    procedure UpdateDisplay;
  end;

var
  FormQuantum: TFormQuantum;

implementation

{$R *.DFM}

procedure TFormQuantum.pbQPaint(Sender: TObject);
var
  R: TRect;
  U, ZX, ZY: Integer;
begin
  pbQ.Canvas.Brush.Style := bsSolid;
  pbQ.Canvas.Brush.Color := clWhite;
  R := pbQ.ClientRect;
  pbQ.Canvas.FillRect(R);

  pbQ.Canvas.Pen.Style := psSolid;
  pbQ.Canvas.Pen.Color := clBlack;
  pbQ.Canvas.Pen.Width := 1;
  pbQ.Canvas.Ellipse(R);

  pbQ.Canvas.MoveTo(0, R.Bottom div 2);
  pbQ.Canvas.LineTo(R.Right, R.Bottom div 2);
  pbQ.Canvas.MoveTo(R.Right div 2, 0);
  pbQ.Canvas.LineTo(R.Right div 2, R.Bottom);

  // 单位 1 的长度是方框边长一半
  U := R.Right div 2;
  ZX := U;
  ZY := U;

  pbQ.Canvas.Pen.Width := 3;
  pbQ.Canvas.MoveTo(ZX, ZY);
  pbQ.Canvas.Pen.Color := clRed;
  pbQ.Canvas.LineTo(ZX + Trunc(U * FQ.Alpha.R), ZY - Trunc(U * FQ.Alpha.I));

  pbQ.Canvas.MoveTo(ZX, ZY);
  pbQ.Canvas.Pen.Color := clBlue;
  pbQ.Canvas.LineTo(ZX + Trunc(U * FQ.Beta.R), ZY - Trunc(U * FQ.Beta.I));
end;

procedure TFormQuantum.FormCreate(Sender: TObject);
begin
  FQ := TCnQuBit.Create(0.7071067, 0, 0, 0.7071067);
  UpdateDisplay;

  FOutQ := TCnQuBit.CreateAsOne;
end;

procedure TFormQuantum.FormDestroy(Sender: TObject);
begin
  FOutQ.Free;
  FQ.Free;
end;

procedure TFormQuantum.btnSetThetaClick(Sender: TObject);
var
  F: Extended;
begin
  F := StrToFloat(edtSetTheta.Text);
  FQ.Theta := F;
  UpdateDisplay;
end;

procedure TFormQuantum.btnSetPhiClick(Sender: TObject);
var
  F: Extended;
begin
  F := StrToFloat(edtSetPhi.Text);
  FQ.Phi := F;
  UpdateDisplay;
end;

procedure TFormQuantum.UpdateDisplay;
begin
  pbQ.Invalidate;
  lblQ.Caption := FQ.ToString;
end;

procedure TFormQuantum.btnHadamardClick(Sender: TObject);
begin
  CnQuBitHadamardGate(FQ, FOutQ);
  lblOutQ.Caption := FOutQ.ToString;
end;

procedure TFormQuantum.btnPauliXClick(Sender: TObject);
begin
  CnQuBitPauliXGate(FQ, FOutQ);
  lblOutQ.Caption := FOutQ.ToString;
end;

procedure TFormQuantum.btnPauliYClick(Sender: TObject);
begin
  CnQuBitPauliYGate(FQ, FOutQ);
  lblOutQ.Caption := FOutQ.ToString;
end;

procedure TFormQuantum.btnPauliZClick(Sender: TObject);
begin
  CnQuBitPauliZGate(FQ, FOutQ);
  lblOutQ.Caption := FOutQ.ToString;
end;

procedure TFormQuantum.btnPhaseShiftClick(Sender: TObject);
begin
  CnQuBitPhaseShiftGate(FQ, FOutQ, StrToFloat(edtNewTheta.Text));
  lblOutQ.Caption := FOutQ.ToString;
end;

end.
