unit UnitMDNS;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls,
  CnMulticastDNS, CnNetwork;

type
  TFormMDNS = class(TForm)
    BtnStart: TButton;
    BtnBrowse: TButton;
    BtnRegister: TButton;
    MemoLog: TMemo;
    EditType: TEdit;
    EditInstance: TEdit;
    EditPort: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure BtnStartClick(Sender: TObject);
    procedure BtnBrowseClick(Sender: TObject);
    procedure BtnRegisterClick(Sender: TObject);
  private
    FMDNS: TCnMulticastDNS;
    procedure OnServiceAdded(Sender: TObject; const Service: TCnMDNSService);
    procedure OnServiceRemoved(Sender: TObject; const Service: TCnMDNSService);
  public
  end;

var
  FormMDNS: TFormMDNS;

implementation

{$R *.dfm}

procedure TFormMDNS.FormCreate(Sender: TObject);
begin
  FMDNS := TCnMulticastDNS.Create(Self);
  FMDNS.OnServiceAdded := OnServiceAdded;
  FMDNS.OnServiceRemoved := OnServiceRemoved;
  EditType.Text := '_http._tcp.local';
  EditInstance.Text := 'Test Service._http._tcp.local';
  EditPort.Text := '8080';
  BtnStart.Caption := 'Start';
  BtnBrowse.Enabled := False;
  BtnRegister.Enabled := False;
end;

procedure TFormMDNS.BtnStartClick(Sender: TObject);
begin
  if not FMDNS.Active then
  begin
    FMDNS.Active := True;
    BtnStart.Caption := 'Stop';
    BtnBrowse.Enabled := True;
    BtnRegister.Enabled := True;
    MemoLog.Lines.Add('mDNS started');
  end
  else
  begin
    FMDNS.Active := False;
    BtnStart.Caption := 'Start';
    BtnBrowse.Enabled := False;
    BtnRegister.Enabled := False;
    MemoLog.Lines.Add('mDNS stopped');
  end;
end;

procedure TFormMDNS.BtnBrowseClick(Sender: TObject);
begin
  FMDNS.Browse(EditType.Text);
end;

procedure TFormMDNS.BtnRegisterClick(Sender: TObject);
var
  Svc: TCnMDNSService;
begin
  Svc.Instance := EditInstance.Text;
  Svc.TypeName := EditType.Text;
  Svc.Domain := 'local';
  Svc.Host := 'local-host.local';
  Svc.Port := StrToIntDef(EditPort.Text, 8080);
  SetLength(Svc.TxtRaw, 0);
  FMDNS.RegisterService(Svc);
  ShowMessage(Format('Register sent: %s %s:%d', [Svc.Instance, Svc.Host, Svc.Port]));
end;

procedure TFormMDNS.OnServiceAdded(Sender: TObject; const Service: TCnMDNSService);
var
  Src: string;
begin
  if Service.Local then
    Src := 'Local'
  else
    Src := 'Remote';
  MemoLog.Lines.Add(Format('Added: %s %s:%d [%s]',
    [Service.Instance, Service.Host, Service.Port, Src]));
end;

procedure TFormMDNS.OnServiceRemoved(Sender: TObject; const Service: TCnMDNSService);
var
  Src: string;
begin
  if Service.Local then
    Src := 'Local'
  else
    Src := 'Remote';
  MemoLog.Lines.Add(Format('Removed: %s [%s]', [Service.Instance, Src]));
end;

end.
