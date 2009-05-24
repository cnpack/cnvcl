unit frmMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ImgList, ExtCtrls, Menus, DB, ADODB,
  PODO_MEMBERS, PODO_WARNINGS, PODO_RESEARCHS, frmAddOrEdit,
  frmAddWarning, frmAddResearch, ComCtrls, ToolWin, Grids, DBGrids,
  CnDHibernateClasses, CnDHibernateMemData, CnDHibernateQueryAdv,
  CnDHibernateBase, DBClient;

type
  TFormMain = class(TForm)
    imgLst: TImageList;
    gbMember: TGroupBox;
    gbOperation: TGroupBox;
    btnAddMember: TButton;
    btnEditMember: TButton;
    btnDeleteMember: TButton;
    lblSearchEvent: TLabel;
    cbSearchEvent: TComboBox;
    lnlKeyWord: TLabel;
    edtKey: TEdit;
    btnSearch: TButton;
    cbKey: TComboBox;
    btnAddWarn: TButton;
    btnAddReseach: TButton;
    Connection: TADOConnection;
    Qry: TCnDHibernateQueryAdvance;
    Idg: TCnDHibernateIdGenerator;
    dsQry: TDataSource;
    gbPages: TGroupBox;
    btnFirst: TButton;
    btnNext: TButton;
    btnPrior: TButton;
    btnLast: TButton;
    btnGoto: TButton;
    edtPageCode: TEdit;
    lblPage: TLabel;
    tbMain: TToolBar;
    tbAddMember: TToolButton;
    tbEditMember: TToolButton;
    ToolButton3: TToolButton;
    tbDeleteMember: TToolButton;
    ToolButton5: TToolButton;
    tbAddWarn: TToolButton;
    tbAddResearch: TToolButton;
    ToolButton8: TToolButton;
    tbSearch: TToolButton;
    ToolButton10: TToolButton;
    BtnHelp: TToolButton;
    ToolButton12: TToolButton;
    BtnExit: TToolButton;
    DBGrid1: TDBGrid;
    procedure FormCreate(Sender: TObject);
    procedure cbSearchEventSelect(Sender: TObject);
    procedure btnSearchClick(Sender: TObject);
    procedure btnAddMemberClick(Sender: TObject);
    procedure btnEditMemberClick(Sender: TObject);
    procedure btnDeleteMemberClick(Sender: TObject);
    procedure btnAddWarnClick(Sender: TObject);
    procedure btnAddReseachClick(Sender: TObject);
    procedure BtnHelpClick(Sender: TObject);
    procedure BtnExitClick(Sender: TObject);
    // procedure skindataTVDblClick(Sender: TObject);
    procedure btnFirstClick(Sender: TObject);
    procedure btnPriorClick(Sender: TObject);
    procedure btnNextClick(Sender: TObject);
    procedure btnLastClick(Sender: TObject);
    procedure btnGotoClick(Sender: TObject);
    procedure DBGrid1DblClick(Sender: TObject);
  private
    procedure displayMask;
  public
    { Public declarations }
  end;

var
  FormMain          : TFormMain;

implementation

{$R *.dfm}

procedure TFormMain.btnAddMemberClick(Sender: TObject);
begin
  // add member
  with TFormAddOrEdit.Create(Application, fmAdd, nil) do
  begin
    ShowModal;
    Free;
  end;
  Qry.Refresh;
  displayMask;
end;

procedure TFormMain.btnAddReseachClick(Sender: TObject);
var
  member            : TMembers;
  QQC               : string;
begin
  // add research
  member := TMembers(qry.get('Members', 'QQCode', Qry.FieldByName('QQCode').Value));
  QQC := member.QQCode;
  if QQC <> EmptyStr then
  begin
    with TFormAddReseach.Create(Application, member) do
    begin
      ShowModal;
      Free;
    end;
  end;
end;

procedure TFormMain.btnAddWarnClick(Sender: TObject);
var
  member            : TMembers;
  QQC               : string;
begin
  // add warn
  member := TMembers(qry.get('Members', 'QQCode', Qry.FieldByName('QQCode').Value));
  QQC := member.QQCode;
  if QQC <> EmptyStr then
  begin
    with TFormAddWarning.Create(Application, member) do
    begin
      ShowModal;
      Free;
    end;
  end;
end;

procedure TFormMain.btnDeleteMemberClick(Sender: TObject);
var
  member            : TMembers;
  QQC               : string;
begin
  // delete member
  member := TMembers(qry.get('Members', 'QQCode', Qry.FieldByName('QQCode').Value));
  QQC := member.QQCode;
  // delete member table
  if QQC <> EmptyStr then
  begin
    if MessageBox(Handle, '是否要删除指定的会员？', '提示', MB_YESNO or MB_ICONINFORMATION) = IDNO then
      Exit;
    Qry.deleteData('Members', member);
    Qry.Refresh;
    with TADOQuery.Create(nil) do
    begin
      Connection := Self.Connection;
      // delete warnings
      SQL.Text := Format('delete from [Warnings] where [QQCode]=''%s''', [QQC]);
      ExecSQL;
      // delete researchs
      SQL.Text := Format('delete from [Researchs] where [QQCode]=''%s''', [QQC]);
      ExecSQL;
      Free;
    end;
  end;
  displayMask;
end;

procedure TFormMain.btnEditMemberClick(Sender: TObject);
var
  member            : TMembers;
  QQC               : string;
begin
  // edit member
  member := TMembers(qry.get('Members', 'QQCode', Qry.FieldByName('QQCode').Value));
  QQC := member.QQCode;
  if QQC <> EmptyStr then
  begin
    with TFormAddOrEdit.Create(Application, fmEdit, member) do
    begin
      ShowModal;
      Free;
    end;
  end;
  Qry.Refresh;
  displayMask;
end;

procedure TFormMain.BtnExitClick(Sender: TObject);
begin
  // exit
  Close;
end;

procedure TFormMain.btnFirstClick(Sender: TObject);
begin
  Qry.FirstPage;
end;

procedure TFormMain.btnGotoClick(Sender: TObject);
begin
  Qry.CurrentPage := StrToIntDef(edtPageCode.Text, 1);
end;

procedure TFormMain.BtnHelpClick(Sender: TObject);
begin
  // todo: help
end;

procedure TFormMain.btnLastClick(Sender: TObject);
begin
  Qry.LastPage;
end;

procedure TFormMain.btnNextClick(Sender: TObject);
begin
  Qry.NextPage;
end;

procedure TFormMain.btnPriorClick(Sender: TObject);
begin
  Qry.PriorPage;
end;

procedure TFormMain.btnSearchClick(Sender: TObject);
var
  map               : ICnMap;
  hql               : TCnStringBuffer;
begin
  // search record
  map := TCnDHHashMap.Create;
  hql := TCnStringBuffer.Create('select * from [Members] where 1=1');
  case cbSearchEvent.ItemIndex of
    1:
      begin
        hql.Append(' and [QQCode] like :QQCode');
        map.put('QQCode', '%' + edtKey.Text + '%');
      end;

    2:
      begin
        hql.Append(' and [UserName] like :UserName');
        map.put('UserName', '%' + edtKey.Text + '%');
      end;

    3:
      begin
        hql.Append(' and [NameCard] like :NameCard');
        map.put('NameCard', '%' + edtKey.Text + '%');
      end;

    4:
      begin
        hql.Append(' and [GotWarn]=:GotWarn');
        map.put('GotWarn', boolToYN(cbKey.ItemIndex = 0));
      end;

    5:
      begin
        hql.Append(' and [GotResearch]=:GotResearch');
        map.put('GotResearch', boolToYN(cbKey.ItemIndex = 0));
      end;
  end;
  Qry.find(hql.toString, map);
  displayMask;
end;

procedure TFormMain.cbSearchEventSelect(Sender: TObject);
begin
  case cbSearchEvent.ItemIndex of
    0..3: edtKey.BringToFront;
  else
    cbKey.BringToFront;
  end;
end;

procedure TFormMain.DBGrid1DblClick(Sender: TObject);
var
  member            : TMembers;
  QQC               : string;
begin
  // view record
  member := TMembers(qry.get('Members', 'QQCode', Qry.FieldByName('QQCode').Value));
  QQC := member.QQCode;
  if QQC <> EmptyStr then
  begin
    with TFormAddOrEdit.Create(Application, fmView, member) do
    begin
      ShowModal;
      Free;
    end;
  end;
end;

procedure TFormMain.displayMask;
begin
  {
  if Qry.RecordCount = 0 then
    pnlMask.Show
  else
    pnlMask.Hide;
  }
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  Connection.Close;
  Connection.ConnectionString := Format('FILE NAME=%s\Conn.udl',[ExtractFilePath(ParamStr(0))]);
  Connection.Open;
  setAllNumber(edtPageCode);
  Qry.Open;
  // btnSearchClick(nil);
  displayMask;
end;

{
procedure TFormMain.skindataTVDblClick(Sender: TObject);
var
  member            : TMembers;
  QQC               : string;
begin
  // view record
  member := TMembers(qry.get('Members', 'QQCode', Qry.FieldByName('QQCode').Value));
  QQC := member.QQCode;
  if QQC <> EmptyStr then
  begin
    with TFormAddOrEdit.Create(Application, fmView, member) do
    begin
      ShowModal;
      Free;
    end;
  end;
end;
}

end.

