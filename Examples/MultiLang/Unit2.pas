unit Unit2;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, CnLangMgr;

type
  TForm2 = class(TForm)
    lbl1: TLabel;
  private
    { Private declarations }
  protected
    procedure DoCreate; override;
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.DFM}

{ TForm2 }

procedure TForm2.DoCreate;
begin
  inherited;
  CnLanguageManager.TranslateForm(Self);
end;

end.
