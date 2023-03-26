unit UnitEventBus;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, CnEventBus;

type
  TReceiver = class(TInterfacedObject, ICnEventBusReceiver)
  public
    constructor Create;
    destructor Destroy; override;

    procedure OnEvent(Event: TCnEvent);
  end;

  TEventBusForm = class(TForm)
    btnRegister: TButton;
    btnUnRegister: TButton;
    btnPost: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnRegisterClick(Sender: TObject);
    procedure btnUnRegisterClick(Sender: TObject);
    procedure btnPostClick(Sender: TObject);
  private
    { Private declarations }
    FReceiver: ICnEventBusReceiver;
  public
    { Public declarations }
  end;

var
  EventBusForm: TEventBusForm;

implementation

{$R *.DFM}

const
  EVENT_NAME = 'Event_Test';

{ TReceiver }

constructor TReceiver.Create;
begin

end;

destructor TReceiver.Destroy;
begin
  inherited;

end;

procedure TReceiver.OnEvent(Event: TCnEvent);
begin
  ShowMessage('Got an Event. Data: ' + IntToStr(Integer(Event.EventData)) + ', Tag '
    + IntToStr(Integer(Event.EventTag)));
end;

procedure TEventBusForm.FormCreate(Sender: TObject);
begin
  FReceiver := TReceiver.Create;
end;

procedure TEventBusForm.FormDestroy(Sender: TObject);
begin
  FReceiver := nil;
end;

procedure TEventBusForm.btnRegisterClick(Sender: TObject);
begin
  EventBus.RegisterReceiver(FReceiver, [EVENT_NAME]);
  ShowMessage('Receiver Registered.');
end;

procedure TEventBusForm.btnUnRegisterClick(Sender: TObject);
begin
  EventBus.UnRegisterReceiver(FReceiver);
  ShowMessage('Receiver UnRegistered.');
end;

procedure TEventBusForm.btnPostClick(Sender: TObject);
begin
  EventBus.PostEvent(EVENT_NAME, Pointer(3), Pointer(5));
end;

end.
