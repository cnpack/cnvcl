unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, CnCalendar, ComCtrls, TypInfo;

type
  TForm1 = class(TForm)
    lbl1: TLabel;
    edt1: TEdit;
    btn1: TButton;
    lbl2: TLabel;
    edt2: TEdit;
    lbl3: TLabel;
    edt3: TEdit;
    lbl4: TLabel;
    dtp1: TDateTimePicker;
    lbl6: TLabel;
    edt4: TEdit;
    lbl7: TLabel;
    edt5: TEdit;
    lbl8: TLabel;
    edt6: TEdit;
    lbl5: TLabel;
    edt7: TEdit;
    procedure btn1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.btn1Click(Sender: TObject);
var
  RiseTime, TransitTime, SetTime: TDateTime;
  SunType: TSunRiseSetType;

  procedure SetTimeText(AEdit: TEdit; ATime: TDateTime);
  begin
    if ATime < 0 then
      AEdit.Text := 'Never'
    else
      AEdit.Text := TimeToStr(ATime);
  end;
begin
  SunType := GetSunRiseSetTime(dtp1.Date, StrToFloat(edt1.Text), StrToFloat(edt2.Text),
    StrToInt(edt3.Text), RiseTime, TransitTime, SetTime);
  SetTimeText(edt4, RiseTime);
  SetTimeText(edt5, TransitTime);
  SetTimeText(edt6, SetTime);
  edt7.Text := GetEnumName(TypeInfo(TSunRiseSetType), Ord(SunType));
end;

end.
