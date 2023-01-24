unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DB, ADODB, Grids, DBGrids, CnDHibernateClasses;

type
  TForm1 = class(TForm)
    DataSource1: TDataSource;
    ADOConnection1: TADOConnection;
    DBGrid1: TDBGrid;
    DHibernateQuery1: TCnDHibernateQuery;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  with ADOConnection1 do
  begin
    Close;
    ConnectionString := 'Provider=Microsoft.Jet.OLEDB.4.0;Data Source=orz.mdb;Persist Security Info=False';
    Open;
  end;
  DHibernateQuery1.Open;
end;

end.
