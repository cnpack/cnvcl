unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ADODB, DB, Grids, DBGrids, DBClient, Provider,
  CnADOUpdateSQL, DBTables;

type
  TForm1 = class(TForm)
    CnADOUpdateSQL1: TCnADOUpdateSQL;
    DataSetProvider1: TDataSetProvider;
    cds1: TClientDataSet;
    ds1: TDataSource;
    dbgrd1: TDBGrid;
    con1: TADOConnection;
    qry1: TADOQuery;
    btn1: TButton;
    cds1ProductNo: TWideStringField;
    cds1ProductName: TWideStringField;
    cds1ProductSortNo: TWideStringField;
    cds1Remark: TWideStringField;
    cds1ProductSortName: TWideStringField;
    lbl1: TLabel;
    procedure btn1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure DataSetProvider1BeforeUpdateRecord(Sender: TObject;
      SourceDS: TDataSet; DeltaDS: TClientDataSet;
      UpdateKind: TUpdateKind; var Applied: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.btn1Click(Sender: TObject);
begin
  cds1.ApplyUpdates(-1);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  with con1 do
  begin
    Connected := False;
    ConnectionString := 'Provider=Microsoft.Jet.OLEDB.4.0;Data Source=' + ExtractFilePath(ParamStr(0)) + 'CnADOUpdateSQL.mdb;Persist Security Info=False';
  end;
  cds1.Active := True;
end;

end.
