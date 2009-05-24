unit frmViewResearch;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DB,   StdCtrls, DBCtrls, ExtCtrls, PODO_RESEARCHS, Grids, DBGrids,
  CnDHibernateMemData, CnDHibernateQueryAdv, CnDHibernateBase, DBClient;

type
  TFormViewResearch = class(TForm)
    gbView: TGroupBox;
    pnlBtn: TPanel;
    btnClose: TButton;
    mmContext: TDBMemo;
    qryResearch: TCnDHibernateQueryAdvance;
    dsResearch: TDataSource;
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
  FormViewResearch  : TFormViewResearch;

implementation

{$R *.dfm}

{ TFormViewResearch }

procedure TFormViewResearch.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TFormViewResearch.btnDeleteClick(Sender: TObject);
var
  research          : TResearchs;
begin
  // delete reseachs
  research := TResearchs(qryResearch.get('Researchs', 'SelfId', qryResearch.FieldByName('SelfId').Value));
  research.Context := EmptyStr;
  research.Time := 0;
  if research.SelfId <> EmptyStr then
    qryResearch.deleteData('Researchs', research);
  qryResearch.Refresh;
  if qryResearch.RecordCount = 0 then
    Close;
end;

procedure TFormViewResearch.btnFirstClick(Sender: TObject);
begin
  qryResearch.FirstPage;
end;

procedure TFormViewResearch.btnLastClick(Sender: TObject);
begin
  qryResearch.LastPage;
end;

procedure TFormViewResearch.btnNextClick(Sender: TObject);
begin
  qryResearch.NextPage;
end;

procedure TFormViewResearch.btnPriorClick(Sender: TObject);
begin
  qryResearch.PriorPage;
end;

constructor TFormViewResearch.Create(AOwner: TComponent; QQCode: string);
var
  map               : ICnMap;
  hql               : TCnStringBuffer;
begin
  inherited Create(AOwner);
  map := TCnDHHashMap.Create;
  map.put('QQCode', QQCode);
  hql := TCnStringBuffer.Create('select * from [Researchs] where [QQCode]=:QQCode');
  qryResearch.find(hql.toString, map);
end;

end.

