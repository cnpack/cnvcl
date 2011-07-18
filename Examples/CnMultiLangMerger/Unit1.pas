unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Clipbrd;

type
  TCnMultiLangMergeFrm = class(TForm)
    mmo1: TMemo;
    mmo2: TMemo;
    mmo3: TMemo;
    btnMerge: TButton;
    lbl1: TLabel;
    lbl2: TLabel;
    lbl3: TLabel;
    btnCopy: TButton;
    lbl4: TLabel;
    procedure btnMergeClick(Sender: TObject);
    procedure btnCopyClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  CnMultiLangMergeFrm: TCnMultiLangMergeFrm;

implementation

{$R *.dfm}

procedure TCnMultiLangMergeFrm.btnCopyClick(Sender: TObject);
begin
  Clipboard.AsText := mmo3.Lines.Text;
end;

procedure TCnMultiLangMergeFrm.btnMergeClick(Sender: TObject);
var
  sl1, sl2, sl3: TStringList;
  i: Integer;
begin
  sl1 := nil;
  sl2 := nil;
  sl3 := nil;
  try
    sl1 := TStringList.Create;
    sl2 := TStringList.Create;
    sl3 := TStringList.Create;
    sl1.Assign(mmo1.Lines);
    sl2.Assign(mmo2.Lines);
    for i := 0 to sl1.Count - 1 do
    begin
      if sl1[i] <> '' then
        if sl2.IndexOfName(sl1.Names[i]) >= 0 then
          sl3.Add(sl1.Names[i] + '=' + sl2.Values[sl1.Names[i]])
        else
          sl3.Add(sl1[i])
      else
        sl3.Add('');
    end;
    mmo3.Lines.Assign(sl3);
  finally
    sl1.Free;
    sl2.Free;
    sl3.Free;
  end;
end;

end.
