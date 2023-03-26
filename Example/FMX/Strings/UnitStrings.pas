unit UnitStrings;

{$I CnPack.inc}

interface

uses
  {$IFDEF MSWINDOWS} Windows, Messages, {$ENDIF} SysUtils, Classes, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs,
  CnStrings, FMX.ExtCtrls, FMX.StdCtrls, FMX.Memo, FMX.TabControl, FMX.Types,
  FMX.ScrollBox, FMX.Controls.Presentation;

type
  TFormStrings = class(TForm)
    pgcStrings: TTabControl;
    tsStringBuilder: TTabItem;
    grpStringBuilder: TGroupBox;
    lblStringBuilderInfo: TLabel;
    btnBoolean: TButton;
    btnByte: TButton;
    btnChar: TButton;
    btnCurrency: TButton;
    btnDouble: TButton;
    btnSmallint: TButton;
    btnInteger: TButton;
    btnInt64: TButton;
    btnTObject: TButton;
    btnShortint: TButton;
    btnSingle: TButton;
    btnUInt64: TButton;
    btnWord: TButton;
    btnCardinal: TButton;
    btnPAnsiChar: TButton;
    btnCharRepeat: TButton;
    btnStringStartCount: TButton;
    btnFormat: TButton;
    mmoContent: TMemo;
    grpMode: TGroupBox;
    rbAuto: TRadioButton;
    rbAnsi: TRadioButton;
    rbWide: TRadioButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnBooleanClick(Sender: TObject);
    procedure btnByteClick(Sender: TObject);
    procedure btnCharClick(Sender: TObject);
    procedure btnCurrencyClick(Sender: TObject);
    procedure btnDoubleClick(Sender: TObject);
    procedure btnSmallintClick(Sender: TObject);
    procedure btnIntegerClick(Sender: TObject);
    procedure btnInt64Click(Sender: TObject);
    procedure btnShortintClick(Sender: TObject);
    procedure btnSingleClick(Sender: TObject);
    procedure btnUInt64Click(Sender: TObject);
    procedure btnWordClick(Sender: TObject);
    procedure btnCardinalClick(Sender: TObject);
    procedure btnPAnsiCharClick(Sender: TObject);
    procedure btnCharRepeatClick(Sender: TObject);
    procedure btnStringStartCountClick(Sender: TObject);
    procedure btnFormatClick(Sender: TObject);
  private
    FBuilder: TCnStringBuilder;
    procedure rgModeClick(Sender: TObject);
    procedure UpdateContent;
  public
    procedure ShowBuilderInfo;
  end;

var
  FormStrings: TFormStrings;

implementation

{$R *.fmx}

procedure TFormStrings.rgModeClick(Sender: TObject);
begin
  FreeAndNil(FBuilder);
  if (Sender = rbAuto) and rbAuto.IsChecked then
    FBuilder := TCnStringBuilder.Create
  else if (Sender = rbAnsi) and rbAnsi.IsChecked then
    FBuilder := TCnStringBuilder.Create(True)
  else if (Sender = rbWide) and rbWide.IsChecked then
    FBuilder := TCnStringBuilder.Create(False)
end;

procedure TFormStrings.ShowBuilderInfo;
begin
  if FBuilder <> nil then
    lblStringBuilderInfo.Text := Format('Length: %d, Capacity %d',
      [FBuilder.CharLength, FBuilder.CharCapacity]);
end;

procedure TFormStrings.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FBuilder);
end;

procedure TFormStrings.FormCreate(Sender: TObject);
begin
  rbAuto.OnClick := rgModeClick;
  rbAnsi.OnClick := rgModeClick;
  rbWide.OnClick := rgModeClick;
  rgModeClick(rbAuto);
end;

procedure TFormStrings.UpdateContent;
begin
  if FBuilder <> nil then
  begin
    if rbAuto.IsChecked then
      mmoContent.Lines.Text := FBuilder.ToString
    else if rbAnsi.IsChecked then
      mmoContent.Lines.Text := FBuilder.ToAnsiString
    else
      mmoContent.Lines.Text := FBuilder.ToWideString;
  end;
end;

procedure TFormStrings.btnFormatClick(Sender: TObject);
begin
  FBuilder.Append('%u %s', [10086, 'Help']);
  UpdateContent;
end;

procedure TFormStrings.btnDoubleClick(Sender: TObject);
var
  V: Double;
begin
  V := -3.1415926535;
  FBuilder.Append(V);
  UpdateContent;
end;

procedure TFormStrings.btnCurrencyClick(Sender: TObject);
var
  V: Currency;
begin
  V := -2.718281828;
  FBuilder.AppendCurrency(V);
  UpdateContent;
end;

procedure TFormStrings.btnCharClick(Sender: TObject);
var
  V: Char;
begin
  V := #$35;
  FBuilder.Append(V);
  UpdateContent;
end;

procedure TFormStrings.btnByteClick(Sender: TObject);
var
  V: Byte;
begin
  V := 56;
  FBuilder.Append(V);
  UpdateContent;
end;

procedure TFormStrings.btnBooleanClick(Sender: TObject);
var
  V: Boolean;
begin
  V := False;
  FBuilder.Append(V);
  UpdateContent;
end;

procedure TFormStrings.btnSingleClick(Sender: TObject);
var
  V: Single;
begin
  V := -0.00099;
  FBuilder.Append(V);
  UpdateContent;
end;

procedure TFormStrings.btnShortintClick(Sender: TObject);
var
  V: ShortInt;
begin
  V := -127;
  FBuilder.Append(V);
  UpdateContent;
end;

procedure TFormStrings.btnInt64Click(Sender: TObject);
var
  V: Int64;
begin
  V := 123456789009876543;
  FBuilder.Append(V);
  UpdateContent;
end;

procedure TFormStrings.btnIntegerClick(Sender: TObject);
var
  V: Integer;
begin
  V := -98776543;
  FBuilder.Append(V);
  UpdateContent;
end;

procedure TFormStrings.btnSmallintClick(Sender: TObject);
var
  V: SmallInt;
begin
  V := 32767;
  FBuilder.Append(V);
  UpdateContent;
end;

procedure TFormStrings.btnStringStartCountClick(Sender: TObject);
var
  V: string;
begin
  V := 'abcdefgh';
  FBuilder.Append(V, 2, 5);
  UpdateContent;
end;

procedure TFormStrings.btnCharRepeatClick(Sender: TObject);
var
  V: Char;
begin
  V := 'K';
  FBuilder.Append(V, 10);
  UpdateContent;
end;

procedure TFormStrings.btnPAnsiCharClick(Sender: TObject);
var
  V: AnsiString;
begin
  V := 'An AnsiString';
  FBuilder.Append(@V[1]);
  UpdateContent;
end;

procedure TFormStrings.btnCardinalClick(Sender: TObject);
var
  V: Cardinal;
begin
  V := 4128847739;
  FBuilder.Append(V);
  UpdateContent;
end;

procedure TFormStrings.btnWordClick(Sender: TObject);
var
  V: Word;
begin
  V := 65535;
  FBuilder.Append(V);
  UpdateContent;
end;

procedure TFormStrings.btnUInt64Click(Sender: TObject);
{$IFDEF SUPPORT_UINT64}
var
  V: UInt64;
{$ENDIF}
begin
{$IFDEF SUPPORT_UINT64}
  V := 68179830439542334;
  FBuilder.Append(V);
  UpdateContent;
{$ENDIF}
end;

end.
