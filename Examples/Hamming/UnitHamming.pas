unit UnitHamming;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  CnDebug;

type
  TFormHamming = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormHamming: TFormHamming;

implementation

uses
  CnHamming;

{$R *.DFM}

procedure TFormHamming.FormCreate(Sender: TObject);
var
  InBits, OutBits: TBits;
  I: Integer;
begin
  InBits := TBits.Create;
  InBits.Size := 128;
  for I := 0 to InBits.Size - 1 do
    InBits.Bits[I] := I mod 2 = 0;
  CnDebugger.TraceBits(InBits);
  OutBits := TBits.Create;
  CnCalcHammingCode(InBits, OutBits, 8);
  CnDebugger.TraceBits(OutBits);
  OutBits.Bits[35] := not OutBits.Bits[35];
  CnVerifyHammingCode(OutBits, InBits, 8);

  CnDebugger.TraceBits(InBits);

  InBits.Free;
  OutBits.Free;
end;

end.
