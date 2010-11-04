unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, CnIniCfg,
  StdCtrls, ComCtrls;

type
  TCnIniCfgDemo = class(TCnIniCfg)
  protected
    procedure InitDefaultValues; override;
  published
    property TestBool: Boolean index 0 read GetBoolean write SetBoolean default False;
    property Enable: Boolean index 1 read GetBoolean write SetBoolean default True;
    property Checked: Boolean index 2 read GetBoolean write SetBoolean default True;

    property TestInteger: Integer index 0 read GetInteger write SetInteger default 100;
    property Width: Integer index 1 read GetInteger write SetInteger default 500;
    property Height: Integer index 2 read GetInteger write SetInteger default 400;

    property TestDate: Double index 0 read GetDouble write SetDouble;
    property TestTime: Double index 1 read GetDouble write SetDouble;

    property TestString: string index 0 read GetString write SetString;
    property Caption: string index 1 read GetString write SetString;
    property Text: string index 2 read GetString write SetString;
  end;

  TForm1 = class(TForm)
    edtTest: TEdit;
    chkEnable: TCheckBox;
    chkChecked: TCheckBox;
    dtpDate: TDateTimePicker;
    dtpTime: TDateTimePicker;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    FCfg: TCnIniCfgDemo;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

{ TCnIniCfgDemo }

procedure TCnIniCfgDemo.InitDefaultValues;
begin
  inherited;
  AddDefaultValue('TestString', 'TestString');
  AddDefaultValue('Caption', 'Test Caption');
  AddDefaultValue('Text', 'Test Text');
  AddDefaultValue('TestDate', Double(EncodeDate(2008, 3, 23)));
  AddDefaultValue('TestTime', Double(EncodeTime(1, 2, 34, 56)));
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
//  FCfg := TCnIniCfgDemo.Create(HKEY_CURRENT_USER, 'Software\' +
//    ChangeFileExt(ExtractFileName(ParamStr(0)), ''));
  FCfg := TCnIniCfgDemo.Create(ChangeFileExt(ParamStr(0), '.ini'));

  Caption := FCfg.Caption;
  Width := FCfg.Width;
  Height := FCfg.Height;
  edtTest.Text := FCfg.Text;
  chkEnable.Checked := FCfg.Enable;
  chkChecked.Checked := FCfg.Checked;
  dtpDate.DateTime := FCfg.TestDate;
  dtpTime.DateTime := FCfg.TestTime;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FCfg.Caption := Caption;
  FCfg.Width := Width;
  FCfg.Height := Height;
  FCfg.Text := edtTest.Text;
  FCfg.Enable := chkEnable.Checked;
  FCfg.Checked := chkChecked.Checked;
  FCfg.TestDate := dtpDate.DateTime;
  FCfg.TestTime := dtpTime.DateTime;
  FCfg.Free;
end;

end.
