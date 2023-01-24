unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DB, ADODB, StdCtrls, CnDHibernateExport;

type
  TForm1 = class(TForm)
    ADOConnection1: TADOConnection;
    DHibernateExport1: TCnDHibernateExport;
    Button1: TButton;
    lbl1: TLabel;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  ShowMessage(IntToStr(Self.DHibernateExport1.Export));
end;

end.
