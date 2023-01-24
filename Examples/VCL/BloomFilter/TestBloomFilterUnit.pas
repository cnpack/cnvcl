unit TestBloomFilterUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  CnBloomFilter, StdCtrls;

type
  TBloomFilterForm = class(TForm)
    btnGenerate: TButton;
    chkDuplicate: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnGenerateClick(Sender: TObject);
  private
    FBloomFilter: TCnStringBloomFilter;
  public
    { Public declarations }
  end;

var
  BloomFilterForm: TBloomFilterForm;

implementation

{$R *.DFM}

uses
  CnBigNumber, CnRandom;

procedure TBloomFilterForm.FormCreate(Sender: TObject);
begin
  FBloomFilter := TCnStringBloomFilter.Create(bfc10Power7);
end;

procedure TBloomFilterForm.FormDestroy(Sender: TObject);
begin
  FBloomFilter.Free;
end;

procedure TBloomFilterForm.btnGenerateClick(Sender: TObject);
var
  I, Len, OldCount, ExistCount: Integer;
  S: string;
begin
  Screen.Cursor := crHourGlass;
  OldCount := FBloomFilter.Count;
  ExistCount := 0;
  for I := 1 to 100000 do
  begin
    Len := 1 + Trunc(Random * 128);
    SetLength(S, Len);

    CnRandomFillBytes(PAnsiChar(S), Len * SizeOf(Char));
    if chkDuplicate.Checked then
      if FBloomFilter.StrExists(S) then
      begin
        Inc(ExistCount);
        Continue;
      end;
    FBloomFilter.AddString(S);
  end;
  Screen.Cursor := crDefault;
  ShowMessage('Insert OK. ' + IntToStr(FBloomFilter.Count - OldCount)
    + ' Duplicated: ' + IntToStr(ExistCount));
end;

end.
