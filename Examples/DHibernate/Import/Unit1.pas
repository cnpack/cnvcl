unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DB, ADODB, StdCtrls, CnDHibernateImport;

type
  TForm1 = class(TForm)
    DHibernateImport1: TCnDHibernateImport;
    ADOConnection1: TADOConnection;
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
  ShowMessage('导入数据：'+IntToStr(Self.DHibernateImport1.Import)+'条');
end;

end.
