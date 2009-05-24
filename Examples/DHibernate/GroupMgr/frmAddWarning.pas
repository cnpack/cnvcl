unit frmAddWarning;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, PODO_MEMBERS, PODO_WARNINGS;

type
  TFormAddWarning = class(TForm)
    lblQQCode: TLabel;
    lblUserName: TLabel;
    lblNameCard: TLabel;
    lblReason: TLabel;
    edtQQCode: TEdit;
    edtUserName: TEdit;
    edtNameCard: TEdit;
    mmReason: TMemo;
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

procedure TFormAddWarning.btnCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TFormAddWarning.btnOKClick(Sender: TObject);
var
  warn : TWarnings;
begin
  try
    // save warning data
    Member.GotWarn := 'Y';
    FormMain.Qry.saveOrUpdateData('Members', Member, 'QQCode');
    //
    warn := TWarnings.Create;
    FormMain.Idg.IdSeed := 'warning';
    warn.SelfId := FormMain.Idg.getNextId;
    warn.QQCode := Member.QQCode;
    warn.WarnTime := Now;
    warn.Reason := mmReason.Lines.Text;
    FormMain.Qry.saveOrUpdateData('Warnings', warn, 'SelfId');
    Close;
  except
    MessageBox(Handle, '±£´æ¾¯¸æÊ§°Ü£¡', '³ö´í£¡', MB_OK or MB_ICONERROR);
  end;
end;

constructor TFormAddWarning.Create(AOwner: TComponent; mm: TMembers);
begin
  inherited Create(AOwner);
  Member := mm;
  getMemberData;
end;

procedure TFormAddWarning.getMemberData;
begin
  with Member do
  begin
    edtQQCode.Text := QQCode;
    edtUserName.Text := UserName;
    edtNameCard.Text := NameCard;
  end;
end;

end.
