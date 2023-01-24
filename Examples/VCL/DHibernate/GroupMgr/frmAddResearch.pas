unit frmAddResearch;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, PODO_MEMBERS, PODO_RESEARCHS;

type
  TFormAddReseach = class(TForm)
    lblQQCode: TLabel;
    lblUserName: TLabel;
    lblNameCard: TLabel;
    lblReason: TLabel;
    edtQQCode: TEdit;
    edtUserName: TEdit;
    edtNameCard: TEdit;
    mmDisp: TMemo;
    gbOperation: TGroupBox;
    btnOK: TButton;
    btnCancel: TButton;
    procedure btnCancelClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
  private
    Member: TMembers;
    procedure getMemberData;
  public
    constructor Create(AOwner: TComponent; mm: TMembers); reintroduce;
  end;

implementation

uses
  frmMain;

{$R *.dfm}

procedure TFormAddReseach.btnCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TFormAddReseach.btnOKClick(Sender: TObject);
var
  research : TResearchs;
begin
  try
    // save research
    Member.GotResearch := 'Y';
    FormMain.Qry.saveOrUpdateData('Members', Member, 'QQCode');
    //
    research := TResearchs.Create;
    FormMain.Idg.IdSeed := 'research';
    research.SelfId := FormMain.Idg.getNextId;
    research.QQCode := Member.QQCode;
    research.Time := Now;
    research.Context := mmDisp.Lines.Text;
    FormMain.Qry.saveOrUpdateData('Researchs', research, 'SelfId');
    Close;
  except
    MessageBox(Handle, '保存成果失败！', '出错！', MB_OK or MB_ICONERROR);
  end;
end;

constructor TFormAddReseach.Create(AOwner: TComponent; mm: TMembers);
begin
  inherited Create(AOwner);
  Member := mm;
  getMemberData;
end;

procedure TFormAddReseach.getMemberData;
begin
  with Member do
  begin
    edtQQCode.Text := QQCode;
    edtUserName.Text := UserName;
    edtNameCard.Text := NameCard;
  end;
end;

end.
