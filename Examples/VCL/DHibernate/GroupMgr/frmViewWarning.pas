unit frmViewWarning;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DB, ADODB, StdCtrls, CnDHibernateBase,
  DBCtrls, ExtCtrls, PODO_WARNINGS, Grids, DBGrids,
  CnDHibernateMemData, CnDHibernateQueryAdv, DBClient;

type
  TFormViewWarning = class(TForm)
    gbView: TGroupBox;
    qryWarn: TCnDHibernateQueryAdvance;
    dsWarn: TDataSource;
    pnlBtn: TPanel;
    mmReason: TDBMemo;
    btnClose: TButton;
    btnDelete: TButton;
    gbPager: TGroupBox;
    btnFirst: TButton;
    btnPrior: TButton;
    btnNext: TButton;
    btnLast: TButton;
    DBGrid1: TDBGrid;
    procedure btnCloseClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnFirstClick(Sender: TObject);
    procedure btnPriorClick(Sender: TObject);
    procedure btnNextClick(Sender: TObject);
    procedure btnLastClick(Sender: TObject);
  private
    { Private declarations }
  public
    constructor Create(AOwner: TComponent; QQCode: string); reintroduce;
  end;

var
  FormViewWarning   : TFormViewWarning;

implementation

uses
  frmMain;

{$R *.dfm}

procedure TFormViewWarning.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TFormViewWarning.btnDeleteClick(Sender: TObject);
var
  warn              : TWarnings;
begin
  // delete warning
  warn := TWarnings(qryWarn.get('Warnings', 'SelfId', qryWarn.FieldByName('SelfId').Value));
  warn.Reason := EmptyStr;
  warn.WarnTime := 0;
  if warn.SelfId <> EmptyStr then
    qryWarn.deleteData('Warnings', warn);
  qryWarn.Refresh;
  if qryWarn.RecordCount = 0 then
    Close;
end;

procedure TFormViewWarning.btnFirstClick(Sender: TObject);
begin
  qryWarn.FirstPage;
end;

procedure TFormViewWarning.btnLastClick(Sender: TObject);
begin
  qryWarn.LastPage;
end;

procedure TFormViewWarning.btnNextClick(Sender: TObject);
begin
  qryWarn.NextPage;
end;

procedure TFormViewWarning.btnPriorClick(Sender: TObject);
begin
  qryWarn.PriorPage;
end;

constructor TFormViewWarning.Create(AOwner: TComponent; QQCode: string);
var
  map               : ICnMap;
  hql               : TCnStringBuffer;
begin
  inherited Create(AOwner);
  map := TCnDHHashMap.Create;
  map.put('QQCode', QQCode);
  hql := TCnStringBuffer.Create('select * from [Warnings] where [QQCode]=:QQCode');
  qryWarn.find(hql.toString, map);
end;

end.

