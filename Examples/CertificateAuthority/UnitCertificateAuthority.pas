unit UnitCertificateAuthority;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, StdCtrls, CnCertificateAuthority;

type
  TFormCA = class(TForm)
    pgc1: TPageControl;
    tsRequest: TTabSheet;
    grpGenRequest: TGroupBox;
    lblKey: TLabel;
    edtRSAKey: TEdit;
    btnBrowseKey: TButton;
    lblContryName: TLabel;
    edtContryName: TEdit;
    lblStateOrProvinceName: TLabel;
    edtStateOrProvinceName: TEdit;
    lblLocalityName: TLabel;
    edtLocalityName: TEdit;
    lblOrgName: TLabel;
    edtOrgName: TEdit;
    lblOrgUnitName: TLabel;
    edtOrgUnitName: TEdit;
    lblCommonName: TLabel;
    edtCommonName: TEdit;
    edtEmail: TEdit;
    lblEmail: TLabel;
    lblHash: TLabel;
    cbbHash: TComboBox;
    btnGenerateCSR: TButton;
    dlgOpen: TOpenDialog;
    dlgSaveCSR: TSaveDialog;
    grpParse: TGroupBox;
    lblCSR: TLabel;
    edtCSR: TEdit;
    btnBrowseCSR: TButton;
    mmoCSRParse: TMemo;
    tsSign: TTabSheet;
    grpSign: TGroupBox;
    lblSignCSR: TLabel;
    edtSignCSR: TEdit;
    btnSignCSRBrowse: TButton;
    lblRoot: TLabel;
    edtSignKey: TEdit;
    btnSignKeyBrowse: TButton;
    btnSign: TButton;
    grpParseCER: TGroupBox;
    lblCER: TLabel;
    edtCER: TEdit;
    btnBrowseCER: TButton;
    mmoCER: TMemo;
    lblRootCrt: TLabel;
    edtRootCRT: TEdit;
    btnRootCRTBrowse: TButton;
    btnSelfSign: TButton;
    btnParseCSR: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btnBrowseCSRClick(Sender: TObject);
    procedure btnBrowseKeyClick(Sender: TObject);
    procedure btnParseCSRClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormCA: TFormCA;

implementation

{$R *.DFM}

procedure TFormCA.FormCreate(Sender: TObject);
begin
  cbbHash.ItemIndex := 0;
end;

procedure TFormCA.btnBrowseCSRClick(Sender: TObject);
begin
  if dlgOpen.Execute then
    edtCSR.Text := dlgOpen.FileName;
end;

procedure TFormCA.btnBrowseKeyClick(Sender: TObject);
begin
  if dlgOpen.Execute then
    edtRSAKey.Text := dlgOpen.FileName;
end;

procedure TFormCA.btnParseCSRClick(Sender: TObject);
var
  CSR: TCnRSACertificateRequest;
begin
  CSR := TCnRSACertificateRequest.Create;
  if CnCALoadCertificateSignRequestFromFile(edtCSR.Text, CSR) then
  begin
    mmoCSRParse.Clear;
    mmoCSRParse.Lines.Add(CSR.ToString);
  end
  else
    ShowMessage('Parse CSR Failed.');
  CSR.Free;
end;

end.
