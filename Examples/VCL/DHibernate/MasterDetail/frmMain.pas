unit frmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Grids, DBGrids, DB, ADODB,
    
  CnDHibernateClasses, CnDHibernateQueryAdv, DBClient, CnDHibernateSubQueryAdv;

type
  TFormMainDetailDemo = class(TForm)
    DHibernateSubQueryAdvance1: TCnDHibernateSubQueryAdvance;
    DHibernateQueryAdvance1: TCnDHibernateQueryAdvance;
    ADOConnection1: TADOConnection;
    DHibernateIdGenerator1: TCnDHibernateIdGenerator;
    DataSource1: TDataSource;
    DataSource2: TDataSource;
    DBGrid1: TDBGrid;
    Label1: TLabel;
    Label2: TLabel;
    DBGrid2: TDBGrid;
    procedure FormCreate(Sender: TObject);
    procedure DBGrid1CellClick(Column: TColumn);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormMainDetailDemo: TFormMainDetailDemo;

implementation

{$R *.dfm}

procedure TFormMainDetailDemo.FormCreate(Sender: TObject);
begin
  DHibernateQueryAdvance1.Open;
  DHibernateSubQueryAdvance1.MainTablePKValue :=
    DHibernateQueryAdvance1.FieldByName(DHibernateSubQueryAdvance1.MainTablePK).Value;
end;

procedure TFormMainDetailDemo.DBGrid1CellClick(Column: TColumn);
begin
  DHibernateSubQueryAdvance1.MainTablePKValue :=
    DHibernateQueryAdvance1.FieldByName(DHibernateSubQueryAdvance1.MainTablePK).Value;
end;

end.

