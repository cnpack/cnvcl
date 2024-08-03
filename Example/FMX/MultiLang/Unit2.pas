unit Unit2;

interface

uses
  Windows, Messages, SysUtils, Classes, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs,
  FMX.StdCtrls, CnLangMgr, FMX.Types, System.Types, System.UITypes,
  FMX.Controls.Presentation;

type
  TForm2 = class(TForm)
    lbl1: TLabel;
  private
    { Private declarations }
  protected
    constructor Create(AOwner: TComponent); override;
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.fmx}

{ TForm2 }

constructor TForm2.Create(AOwner: TComponent);
begin
  inherited;
  CnLanguageManager.TranslateFmxForm(Self);
end;

end.
