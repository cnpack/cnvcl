unit frmAddOrEdit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, PODO_MEMBERS, frmViewWarning, frmViewResearch, CnDHibernateBase;

type

  TFormMode = (fmAdd { 添加会员 }, fmEdit { 修改会员 }, fmView { 浏览会员 });
  
  TFormAddOrEdit = class(TForm)
    lblQQCode: TLabel;
    lblUserName: TLabel;
    lblSex: TLabel;
    edtQQCode: TEdit;
    edtUserName: TEdit;
    edtAge: TEdit;
    cbSex: TComboBox;
    lblAge: TLabel;
    lblArea: TLabel;
    lblIdCard: TLabel;
    lblEmail: TLabel;
    lblWebsite: TLabel;
    lblStat: TLabel;
    lblIden: TLabel;
    lblResearch: TLabel;
    lblOutReason: TLabel;
    edtArea: TEdit;
    edtIdCard: TEdit;
    edtEmail: TEdit;
    edtWebSite: TEdit;
    cbStatus: TComboBox;
    cbIdentity: TComboBox;
    mmResearch: TMemo;
    mmOutReason: TMemo;
    gbOperation: TGroupBox;
    btnOK: TButton;
    btnCancel: TButton;
    btnViewWarn: TButton;
    btnViewResearch: TButton;
    lblInTime: TLabel;
    lblOutTime: TLabel;
    edtInTime: TEdit;
    edtOutTime: TEdit;
    procedure btnCancelClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnViewWarnClick(Sender: TObject);
    procedure btnViewResearchClick(Sender: TObject);
  private
    FormMode: TFormMode;
    Member: TMembers;
    FInTime: TDateTime;
    procedure getMemberData;
    procedure setComponentReadonly;
  public
    constructor Create(AOwner: TComponent; fm: TFormMode; mb: TMembers); reintroduce;
  end;

implementation

uses
  frmMain;

{$R *.dfm}

procedure TFormAddOrEdit.btnCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TFormAddOrEdit.btnOKClick(Sender: TObject);
var
  mm: TMembers;
begin
  // save or update
  try
    mm := TMembers.Create;
    with mm do
    begin
      QQCode := edtQQCode.Text;
      UserName := edtUserName.Text;

      if cbSex.ItemIndex = 1 then
        Sex := 'M'
      else
        if cbSex.ItemIndex = 2 then
          Sex := 'F'
        else
          Sex := EmptyStr;
      Age := edtAge.Text;
      Area := edtArea.Text;
      NameCard := edtIdCard.Text;
      Email := edtEmail.Text;
      WebSite := edtWebSite.Text;
      Research := mmResearch.Lines.Text;

      if cbStatus.ItemIndex = 0 then
        Status := 'In'
      else
        Status := 'Out';

      OutReason := mmOutReason.Lines.Text;
      if cbIdentity.ItemIndex = 0 then
        Identity := 'Member'
      else
        Identity := 'Admin';
      if FormMode = fmAdd then
        InTime := Now
      else if FormMode = fmEdit then
        InTime := FInTime;
      if Status = 'Out' then
        OutTime := Now;
      
    end;
    FormMain.Qry.saveOrUpdateData('Members', mm, 'QQCode');
    Close;
  except
    on E: Exception do
      MessageBox(Handle, PChar(E.Message), '出错！', MB_OK or MB_ICONERROR);
  end;
end;

procedure TFormAddOrEdit.btnViewResearchClick(Sender: TObject);
begin
  // view researchs
  with TFormViewResearch.Create(Application, edtQQCode.Text) do
  begin
    if qryResearch.RecordCount = 0 then
      MessageBox(Handle, '该会员没有成果记录！', '提示', MB_OK or MB_ICONINFORMATION)
    else
      ShowModal;
    Free;
  end;
end;

procedure TFormAddOrEdit.btnViewWarnClick(Sender: TObject);
begin
  // view warns
  with TFormViewWarning.Create(Application, edtQQCode.Text) do
  begin
    if qryWarn.RecordCount = 0 then
      MessageBox(Handle, '该会员没有警告记录！', '提示', MB_OK or MB_ICONINFORMATION)
    else
      ShowModal;
    Free;
  end;  
end;

constructor TFormAddOrEdit.Create(AOwner: TComponent; fm: TFormMode; mb: TMembers);
begin
  inherited Create(AOwner);
  FormMode := fm;
  Member := mb;
  if fm = fmEdit then
  begin
    edtQQCode.ReadOnly := True;
    edtQQCode.Color := clSilver;
    getMemberData;
  end;
  if fm = fmView then
  begin
    getMemberData;
    setComponentReadonly;
    btnOK.Hide;
    btnViewWarn.Show;
    btnViewResearch.Show;
    lblInTime.Show;
    lblOutTime.Show;
    edtInTime.Show;
    edtOutTime.Show;
    btnCancel.Caption := '关闭';
    Height := 390;
  end;  
end;

procedure TFormAddOrEdit.FormCreate(Sender: TObject);
begin
  if FormMode = fmAdd then
    Caption := '添加会员'
  else if formmode = fmEdit then
    Caption := '修改会员'
  else
    Caption := '浏览会员资料';
  setAllNumber(edtQQCode);
  setAllNumber(edtAge);
end;

procedure TFormAddOrEdit.getMemberData;
begin
  // get member data
  with Member do
  begin
    edtQQCode.Text := QQCode;
    edtUserName.Text := UserName;
    if Sex = 'M' then
      cbSex.ItemIndex := 1
    else
      if Sex = 'F' then
        cbSex.ItemIndex := 2
      else
        cbSex.ItemIndex := 0;
    edtAge.Text := Age;
    edtArea.Text := Area;
    edtIdCard.Text := NameCard;
    edtEmail.Text := Email;
    edtWebSite.Text := WebSite;
    mmResearch.Lines.Text := Research;
    if Status = 'In' then
      cbStatus.ItemIndex := 0
    else
      cbStatus.ItemIndex := 1;
    mmOutReason.Lines.Text := OutReason;
    if Identity = 'Admin' then
      cbIdentity.ItemIndex := 1
    else
      cbIdentity.ItemIndex := 0;
    FInTime := InTime;
    try
      if InTime = 0 then
        edtInTime.Text := EmptyStr
      else
        edtInTime.Text := DateTimeToStr(InTime);
    except
      edtInTime.Text := EmptyStr;
    end;  
    try
      if OutTime = 0 then
        edtOutTime.Text := EmptyStr
      else
        edtOutTime.Text := DateTimeToStr(OutTime);
    except
      edtOutTime.Text := EmptyStr;
    end;  
  end;
end;

procedure TFormAddOrEdit.setComponentReadonly;
var
  i: Integer;
begin
  // set readonly..
  for i := 0 to ControlCount - 1 do
  begin
    if Controls[i] is TEdit then
      TEdit(Controls[i]).Enabled := False;
    if Controls[i] is TMemo then
      TMemo(Controls[i]).Enabled := False;
    if Controls[i]  is TComboBox then
    begin
      TComboBox(Controls[i]).Style := csSimple;
      TComboBox(Controls[i]).Enabled := False;
    end;
  end;
end;


end.

