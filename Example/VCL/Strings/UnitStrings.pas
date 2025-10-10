unit UnitStrings;

{$I CnPack.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, CnStrings, ExtCtrls, StdCtrls;

type
  TFormStrings = class(TForm)
    pgcStrings: TPageControl;
    tsStringBuilder: TTabSheet;
    grpStringBuilder: TGroupBox;
    rgMode: TRadioGroup;
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
    tsStrings: TTabSheet;
    btnTestPosEx: TButton;
    mmoStringsRes: TMemo;
    procedure rgModeClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnFormatClick(Sender: TObject);
    procedure btnDoubleClick(Sender: TObject);
    procedure btnCurrencyClick(Sender: TObject);
    procedure btnCharClick(Sender: TObject);
    procedure btnByteClick(Sender: TObject);
    procedure btnBooleanClick(Sender: TObject);
    procedure btnSingleClick(Sender: TObject);
    procedure btnShortintClick(Sender: TObject);
    procedure btnInt64Click(Sender: TObject);
    procedure btnIntegerClick(Sender: TObject);
    procedure btnSmallintClick(Sender: TObject);
    procedure btnStringStartCountClick(Sender: TObject);
    procedure btnCharRepeatClick(Sender: TObject);
    procedure btnPAnsiCharClick(Sender: TObject);
    procedure btnCardinalClick(Sender: TObject);
    procedure btnWordClick(Sender: TObject);
    procedure btnUInt64Click(Sender: TObject);
    procedure btnTestPosExClick(Sender: TObject);
  private
    FBuilder: TCnStringBuilder;
    procedure UpdateContent;
  public
    procedure ShowBuilderInfo;
  end;

var
  FormStrings: TFormStrings;

implementation

{$R *.DFM}

procedure TFormStrings.rgModeClick(Sender: TObject);
begin
  FreeAndNil(FBuilder);
  case rgMode.ItemIndex of
    0: FBuilder := TCnStringBuilder.Create;
    1: FBuilder := TCnStringBuilder.Create(True);
    2: FBuilder := TCnStringBuilder.Create(False);
  end;
end;

procedure TFormStrings.ShowBuilderInfo;
begin
  if FBuilder <> nil then
    lblStringBuilderInfo.Caption := Format('Length: %d, Capacity %d',
      [FBuilder.CharLength, FBuilder.CharCapacity]);
end;

procedure TFormStrings.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FBuilder);
end;

procedure TFormStrings.FormCreate(Sender: TObject);
begin
  rgMode.OnClick(rgMode);
end;

procedure TFormStrings.UpdateContent;
begin
  if FBuilder <> nil then
  begin
    case rgMode.ItemIndex of
      0: mmoContent.Lines.Text := FBuilder.ToString;
      1: mmoContent.Lines.Text := FBuilder.ToAnsiString;
      2: mmoContent.Lines.Text := FBuilder.ToWideString;
    end;
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

procedure TFormStrings.btnTestPosExClick(Sender: TObject);
var
  P: Integer;
begin
  // 1. �������ܲ���
  mmoStringsRes.Lines.Add('=== �������ܲ��� ===');
  P := CnPosEx('abc', '123abc456', False, False, 1);
  mmoStringsRes.Lines.Add('����"abc"��"123abc456"�е�λ��(�����ִ�Сд,������): ' + IntToStr(P)); // Ӧ���� 4

  // 2. ��Сд���в���
  mmoStringsRes.Lines.Add('=== ��Сд���в��� ===');
  P := CnPosEx('ABC', '123abc456', True, False, 1);
  mmoStringsRes.Lines.Add('���ִ�Сд����"ABC": ' + IntToStr(P)); // Ӧ���� 0
  P := CnPosEx('ABC', '123abc456', False, False, 1);
  mmoStringsRes.Lines.Add('�����ִ�Сд����"ABC": ' + IntToStr(P)); // Ӧ���� 4

  // 3. ����ƥ�����
  mmoStringsRes.Lines.Add('=== ����ƥ����� ===');
  P := CnPosEx('abc', '123abc456 abc 789', False, True, 1);
  mmoStringsRes.Lines.Add('����ƥ�����"abc": ' + IntToStr(P)); // Ӧ���� 11
  P := CnPosEx('abc', '123abc456 abc 789', False, False, 1);
  mmoStringsRes.Lines.Add('������ƥ�����"abc": ' + IntToStr(P)); // Ӧ���� 4

  // 4. ��γ��ֲ���
  mmoStringsRes.Lines.Add('=== ��γ��ֲ��� ===');
  P := CnPosEx('abc', 'abc123abc456abc', False, False, 2);
  mmoStringsRes.Lines.Add('���ҵ�2�γ���: ' + IntToStr(P)); // Ӧ���� 7
  P := CnPosEx('abc', 'abc123abc456abc', False, False, 3);
  mmoStringsRes.Lines.Add('���ҵ�3�γ���: ' + IntToStr(P)); // Ӧ���� 13

  // 5. ���ֲ���
  mmoStringsRes.Lines.Add('=== ���ֲ��� ===');
  P := CnPosEx('����', '����һ�����Ժ���', False, False, 1);
  mmoStringsRes.Lines.Add('���Һ���"����": ' + IntToStr(P)); // Ӧ���� 9 �� 5
  P := CnPosEx('����', '����һ�����Ժ�������', False, True, 2);
  mmoStringsRes.Lines.Add('����ƥ����ҵ�2��"����": ' + IntToStr(P)); // Ӧ���� 17 �� 9

  // 6. �߽���������
  mmoStringsRes.Lines.Add('=== �߽��������� ===');
  P := CnPosEx('', '123abc456', False, False, 1);
  mmoStringsRes.Lines.Add('���Ӵ�����: ' + IntToStr(P)); // Ӧ����0
  P := CnPosEx('abc', '', False, False, 1);
  mmoStringsRes.Lines.Add('���ַ�������: ' + IntToStr(P)); // Ӧ����0
  P := CnPosEx('abc', '123ABC456', True, False, 1);
  mmoStringsRes.Lines.Add('�������Ӵ�����(���ִ�Сд): ' + IntToStr(P)); // Ӧ����0
end;

end.
