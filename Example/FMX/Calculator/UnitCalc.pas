unit UnitCalc;

interface

uses
  {$IFDEF MSWINDOWS} Windows, Messages, {$ENDIF} SysUtils, Classes, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs,
  FMX.StdCtrls, FMX.ExtCtrls, CnCommon, FMX.Edit, FMX.Types,
  FMX.Controls.Presentation;

type
  TFormCalc = class(TForm)
    lblExpression: TLabel;
    lblResult: TLabel;
    lblExpr: TLabel;
    edtExpr: TEdit;
    edtResult: TEdit;
    pnlButtons: TPanel;
    btnClear: TButton;
    btnBackSpace: TButton;
    btn1: TButton;
    btn2: TButton;
    btn3: TButton;
    btn4: TButton;
    btn5: TButton;
    btn6: TButton;
    btn7: TButton;
    btn8: TButton;
    btn9: TButton;
    btn0: TButton;
    btnDot: TButton;
    btnRevert: TButton;
    btnAdd: TButton;
    btnSub: TButton;
    btnMul: TButton;
    btnDiv: TButton;
    btnEnter: TButton;
    procedure btnClearClick(Sender: TObject);
    procedure btnBackSpaceClick(Sender: TObject);
    procedure btn1Click(Sender: TObject);
    procedure btnRevertClick(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure btnEnterClick(Sender: TObject);
  private
    { Private declarations }
    FExpression: string;
    FLastFactor: string;
    FLastOperator: string;
    FCurrentIsFactor: Boolean;
    FCurrentIsOperator: Boolean;
    procedure ClearCalc;
    procedure AppendFactor(const S: string);
    procedure AppendOperator(const S: string);
    procedure SetExpression(const Value: string);
  public
    { Public declarations }
    property Expression: string read FExpression write SetExpression;
  end;

var
  FormCalc: TFormCalc;

implementation

{$R *.fmx}

procedure TFormCalc.AppendOperator(const S: string);
begin
  if FCurrentIsFactor then
    Expression := FExpression + FLastFactor;
  FCurrentIsFactor := False;

  FLastFactor := '';
  FLastOperator := S;
  FCurrentIsOperator := True;
  edtResult.Text := '';
end;

procedure TFormCalc.AppendFactor(const S: string);
begin
  if FCurrentIsOperator then
    Expression := FExpression + FLastOperator;
  FCurrentIsOperator := False;

  FLastFactor := FLastFactor + S;
  edtExpr.Text := FLastFactor;
  FCurrentIsFactor := True;
  edtResult.Text := '';
end;

procedure TFormCalc.btnClearClick(Sender: TObject);
begin
  ClearCalc;
end;

procedure TFormCalc.ClearCalc;
begin
  FLastFactor := '';
  FLastOperator := '';
  FExpression := '';
  FCurrentIsFactor := True;
  FCurrentIsOperator := False;
  edtExpr.Text := '';
//  lblExpr.Text := '';
//  edtResult.Text := '';
end;

procedure TFormCalc.btnAddClick(Sender: TObject);
begin
  AppendOperator((Sender as TButton).Text);
end;

procedure TFormCalc.btn1Click(Sender: TObject);
begin
  AppendFactor((Sender as TButton).Text);
end;

procedure TFormCalc.btnBackSpaceClick(Sender: TObject);
begin
  if FCurrentIsFactor then
    if FLastFactor <> '' then
    begin
      Delete(FLastFactor, Length(FLastFactor), 1);
      edtExpr.Text := FLastFactor;
    end;
end;

procedure TFormCalc.btnRevertClick(Sender: TObject);
begin
  if FCurrentIsFactor then
    if FLastFactor <> '' then
    begin
      if FLastFactor[1] = '-' then
        Delete(FLastFactor, 1, 1)
      else
        FLastFactor := '-' + FLastFactor;
      edtExpr.Text := FLastFactor;
    end;
end;

procedure TFormCalc.btnEnterClick(Sender: TObject);
begin
  if FCurrentIsFactor then
  begin
    Expression := FExpression + FLastFactor;
    edtResult.Text := FloatToStr(EvalSimpleExpression(FExpression));
    ClearCalc;
  end;
end;

procedure TFormCalc.SetExpression(const Value: string);
begin
  FExpression := Value;
  lblExpr.Text := FExpression;
end;

end.
