unit FMainUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, CnMulticastEvent;

type
  TfrmMain = class(TForm)
    EHF: TButton;
    AEH1: TButton;
    REH1: TButton;
    AEH2: TButton;
    REH2: TButton;
    AEH3: TButton;
    REH3: TButton;
    AEH4: TButton;
    REH4: TButton;
    CAEH: TButton;
    mmoLog: TMemo;
    chbAutoFire: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure EHFClick(Sender: TObject);
    procedure AddEventHandlers(Sender: TObject);
    procedure RemoveEventHandlers(Sender: TObject);
    procedure CAEHClick(Sender: TObject);
    procedure chbAutoFireClick(Sender: TObject);
  private
    { Private declarations }
    MC: TCnMulticastEvent;
    EH: array[0..3] of Pointer;

    procedure AddOnClickListener;
    procedure AutoClickEHF(Sender: TObject);
    procedure OnClickListener(Sender: TObject);
    procedure InitMEs;

    procedure ClearLog;
    procedure Log(const s: string);

    procedure EH1(Sender: TObject);
    procedure EH2(Sender: TObject);
    procedure EH3(Sender: TObject);
    procedure EH4(Sender: TObject);
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

{ TForm3 }

procedure TfrmMain.AddOnClickListener;
var
  i: Integer;
begin
  for i := 0 to ControlCount - 1 do
    if (Controls[i] is TButton) then
      CnMulticastEventManager[@@TButton(Controls[i]).OnClick].Add(CnMakePMethod(@TfrmMain.OnClickListener, Self));
end;

procedure TfrmMain.AddEventHandlers(Sender: TObject);
begin
  MC.Add(CnMakePMethod(EH[TButton(Sender).Tag], Self));
end;

procedure TfrmMain.AutoClickEHF(Sender: TObject);
begin
  EHF.Click;
end;

procedure TfrmMain.CAEHClick(Sender: TObject);
begin
  MC.Clear;
  MC.Add(CnMakePMethod(@TfrmMain.EHFClick, Self));
  MC.Add(CnMakePMethod(@TfrmMain.OnClickListener, Self));
end;

procedure TfrmMain.chbAutoFireClick(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to ControlCount - 1 do
    if (Controls[i] is TButton) and (Controls[i] <> EHF) then
      if chbAutoFire.Checked then
        CnMulticastEventManager[@@TButton(Controls[i]).OnClick].Insert(1, CnMakePMethod(@TfrmMain.AutoClickEHF, Self))
      else
        CnMulticastEventManager[@@TButton(Controls[i]).OnClick].Remove(@TfrmMain.AutoClickEHF, Self);
  if chbAutoFire.Checked then AutoClickEHF(chbAutoFire);
end;

procedure TfrmMain.ClearLog;
begin
  mmoLog.Clear;
end;

procedure TfrmMain.EH1(Sender: TObject);
begin
  Log('Event Handler 1');
end;

procedure TfrmMain.EH2(Sender: TObject);
begin
  Log('Event Handler 2');
end;

procedure TfrmMain.EH3(Sender: TObject);
begin
  Log('Event Handler 3');
end;

procedure TfrmMain.EH4(Sender: TObject);
begin
  Log('Event Handler 4');
end;

procedure TfrmMain.EHFClick(Sender: TObject);
begin
  ClearLog;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  EH[0] := @TfrmMain.EH1;
  EH[1] := @TfrmMain.EH2;
  EH[2] := @TfrmMain.EH3;
  EH[3] := @TfrmMain.EH4;
  InitMEs;
  MC := TCnMulticastEvent.Create(@@EHF.OnClick, TypeInfo(TNotifyEvent));
  AddOnClickListener;
  if chbAutoFire.Checked then chbAutoFireClick(chbAutoFire);
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
// if MC freed after MEMgr.Item.Free then event handlers after MC will lost
//  MEMgr.Remove(MC.SavedMethodPointer, True);
  MC.Free;
end;

procedure TfrmMain.InitMEs;
var
  i: Integer;
begin
  for i := 0 to ControlCount - 1 do
    if (Controls[i] is TButton) then
      CnMulticastEventManager.Add(@@TButton(Controls[i]).OnClick, TypeInfo(TNotifyEvent));
end;

procedure TfrmMain.Log(const s: string);
begin
  mmoLog.Lines.Add(s);
end;

procedure TfrmMain.OnClickListener(Sender: TObject);
begin
  if Sender <> nil then Log('(' + TButton(Sender).Caption + ') is Clicked.');
end;

procedure TfrmMain.RemoveEventHandlers(Sender: TObject);
begin
  MC.Remove(EH[TButton(Sender).Tag], Self);
end;

end.
