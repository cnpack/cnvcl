unit UnitNative;

interface

{$I CnPack.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, CnNativeDecl, ExtCtrls;

type
  TFormNative = class(TForm)
    btnUInt64Div: TButton;
    btnUInt64Mod: TButton;
    mmoRes: TMemo;
    bvl1: TBevel;
    btnStrUInt64: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btnUInt64DivClick(Sender: TObject);
    procedure btnUInt64ModClick(Sender: TObject);
    procedure btnStrUInt64Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormNative: TFormNative;

implementation

{$R *.DFM}

const
  A0: TUInt64 = 0;
  MIN_UINT64: TUInt64 = 0;
  MAX_UINT64: TUInt64 = $FFFFFFFFFFFFFFFF; // 18446744073709551615;
  MIN_INT64: TUInt64 = $8000000000000000;  // -9223372036854775808;
  MAX_INT64: TUInt64 = $7FFFFFFFFFFFFFFF; // 9223372036854775807;

var
  A1, A2, A3, A4, A5, A6, A7, A8, B1, B2, B3, B4, B5, B6, B7, B8: TUInt64;

procedure TFormNative.FormCreate(Sender: TObject);
begin
  // UInt64 的 div mod 各包括四类测试用例（从 Int64 的角度来看）：A+B+、A-B-、A+B-、A-B+

  A1 := TUInt64(4227372036857772807);
  A2 := TUInt64(-2227372036857772807); // 16219372036851778809
  A3 := TUInt64(97372037857779845);
  A4 := TUInt64(-97372037857779845);   // 18349372035851771771
  A5 := $22222222FFFFFFFF;
  A6 := $FFFFFFFF22222222;
  A7 := $FEFEFEFEFEFEFEFE;
  A8 := $FEFEFEFEFEFEFEFE;
  B1 := TUInt64(84560395435344);
  B2 := TUInt64(-684560395435342);     // 18446059513314116274
  B3 := TUInt64(-784560395435344);     // 18445959513314116272
  B4 := TUInt64(64560395435344);
  B5 := $1111111111111111;
  B6 := $1111111111111111;
  B7 := $0000000033333333;
  B8 := $3333333300000000;
end;

procedure TFormNative.btnUInt64DivClick(Sender: TObject);
begin
  mmoRes.Clear;
  mmoRes.Lines.Add(UInt64ToStr(A0) + ' div ' + UInt64ToStr(B1) + ' = ' + UInt64ToStr(UInt64Div(A0, B1)));
  mmoRes.Lines.Add(UInt64ToStr(A1) + ' div ' + UInt64ToStr(B1) + ' = ' + UInt64ToStr(UInt64Div(A1, B1)));
  mmoRes.Lines.Add(UInt64ToStr(A2) + ' div ' + UInt64ToStr(B2) + ' = ' + UInt64ToStr(UInt64Div(A2, B2)));
  mmoRes.Lines.Add(UInt64ToStr(A3) + ' div ' + UInt64ToStr(B3) + ' = ' + UInt64ToStr(UInt64Div(A3, B3)));
  mmoRes.Lines.Add(UInt64ToStr(A4) + ' div ' + UInt64ToStr(B4) + ' = ' + UInt64ToStr(UInt64Div(A4, B4)));
  mmoRes.Lines.Add(UInt64ToStr(A5) + ' div ' + UInt64ToStr(B5) + ' = ' + UInt64ToStr(UInt64Div(A5, B5)));
  mmoRes.Lines.Add(UInt64ToStr(A6) + ' div ' + UInt64ToStr(B6) + ' = ' + UInt64ToStr(UInt64Div(A6, B6)));
  mmoRes.Lines.Add(UInt64ToStr(A7) + ' div ' + UInt64ToStr(B7) + ' = ' + UInt64ToStr(UInt64Div(A7, B7)));
  mmoRes.Lines.Add(UInt64ToStr(A8) + ' div ' + UInt64ToStr(B8) + ' = ' + UInt64ToStr(UInt64Div(A8, B8)));
{$IFDEF SUPPORT_UINT64}
  mmoRes.Lines.Add('');
  mmoRes.Lines.Add(UInt64ToStr(A0) + ' div ' + UInt64ToStr(B1) + ' = ' + UInt64ToStr(A0 div B1));
  mmoRes.Lines.Add(UInt64ToStr(A1) + ' div ' + UInt64ToStr(B1) + ' = ' + UInt64ToStr(A1 div B1));
  mmoRes.Lines.Add(UInt64ToStr(A2) + ' div ' + UInt64ToStr(B2) + ' = ' + UInt64ToStr(A2 div B2));
  mmoRes.Lines.Add(UInt64ToStr(A3) + ' div ' + UInt64ToStr(B3) + ' = ' + UInt64ToStr(A3 div B3));
  mmoRes.Lines.Add(UInt64ToStr(A4) + ' div ' + UInt64ToStr(B4) + ' = ' + UInt64ToStr(A4 div B4));
  mmoRes.Lines.Add(UInt64ToStr(A5) + ' div ' + UInt64ToStr(B5) + ' = ' + UInt64ToStr(A5 div B5));
  mmoRes.Lines.Add(UInt64ToStr(A6) + ' div ' + UInt64ToStr(B6) + ' = ' + UInt64ToStr(A6 div B6));
  mmoRes.Lines.Add(UInt64ToStr(A7) + ' div ' + UInt64ToStr(B7) + ' = ' + UInt64ToStr(A7 div B7));
  mmoRes.Lines.Add(UInt64ToStr(A8) + ' div ' + UInt64ToStr(B8) + ' = ' + UInt64ToStr(A8 div B8));
{$ENDIF}
end;

procedure TFormNative.btnUInt64ModClick(Sender: TObject);
begin
  mmoRes.Clear;
  mmoRes.Lines.Add(UInt64ToStr(A0) + ' mod ' + UInt64ToStr(B1) + ' = ' + UInt64ToStr(UInt64Mod(A0, B1)));
  mmoRes.Lines.Add(UInt64ToStr(A1) + ' mod ' + UInt64ToStr(B1) + ' = ' + UInt64ToStr(UInt64Mod(A1, B1)));
  mmoRes.Lines.Add(UInt64ToStr(A2) + ' mod ' + UInt64ToStr(B2) + ' = ' + UInt64ToStr(UInt64Mod(A2, B2)));
  mmoRes.Lines.Add(UInt64ToStr(A3) + ' mod ' + UInt64ToStr(B3) + ' = ' + UInt64ToStr(UInt64Mod(A3, B3)));
  mmoRes.Lines.Add(UInt64ToStr(A4) + ' mod ' + UInt64ToStr(B4) + ' = ' + UInt64ToStr(UInt64Mod(A4, B4)));
  mmoRes.Lines.Add(UInt64ToStr(A5) + ' mod ' + UInt64ToStr(B5) + ' = ' + UInt64ToStr(UInt64Mod(A5, B5)));
  mmoRes.Lines.Add(UInt64ToStr(A6) + ' mod ' + UInt64ToStr(B6) + ' = ' + UInt64ToStr(UInt64Mod(A6, B6)));
  mmoRes.Lines.Add(UInt64ToStr(A7) + ' mod ' + UInt64ToStr(B7) + ' = ' + UInt64ToStr(UInt64Mod(A7, B7)));
  mmoRes.Lines.Add(UInt64ToStr(A8) + ' mod ' + UInt64ToStr(B8) + ' = ' + UInt64ToStr(UInt64Mod(A8, B8)));
{$IFDEF SUPPORT_UINT64}
  mmoRes.Lines.Add('');
  mmoRes.Lines.Add(UInt64ToStr(A0) + ' mod ' + UInt64ToStr(B1) + ' = ' + UInt64ToStr(A0 mod B1));
  mmoRes.Lines.Add(UInt64ToStr(A1) + ' mod ' + UInt64ToStr(B1) + ' = ' + UInt64ToStr(A1 mod B1));
  mmoRes.Lines.Add(UInt64ToStr(A2) + ' mod ' + UInt64ToStr(B2) + ' = ' + UInt64ToStr(A2 mod B2));
  mmoRes.Lines.Add(UInt64ToStr(A3) + ' mod ' + UInt64ToStr(B3) + ' = ' + UInt64ToStr(A3 mod B3));
  mmoRes.Lines.Add(UInt64ToStr(A4) + ' mod ' + UInt64ToStr(B4) + ' = ' + UInt64ToStr(A4 mod B4));
  mmoRes.Lines.Add(UInt64ToStr(A5) + ' mod ' + UInt64ToStr(B5) + ' = ' + UInt64ToStr(A5 mod B5));
  mmoRes.Lines.Add(UInt64ToStr(A6) + ' mod ' + UInt64ToStr(B6) + ' = ' + UInt64ToStr(A6 mod B6));
  mmoRes.Lines.Add(UInt64ToStr(A7) + ' mod ' + UInt64ToStr(B7) + ' = ' + UInt64ToStr(A7 mod B7));
  mmoRes.Lines.Add(UInt64ToStr(A8) + ' mod ' + UInt64ToStr(B8) + ' = ' + UInt64ToStr(A8 mod B8));
{$ENDIF}
end;

procedure TFormNative.btnStrUInt64Click(Sender: TObject);
var
  A: TUInt64;
  S: string;
begin
  S := '10977225559701242671';
  A := StrToUInt64(S);
  mmoRes.Lines.Clear;
  mmoRes.Lines.Add(S + ' Converted to TUInt64.');
  mmoRes.Lines.Add(UInt64ToStr(A));
end;

end.

