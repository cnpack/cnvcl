unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, CnVarList;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    procedure Print(Text: string);
  public
   { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var
  list: TCnVarList;
  err: string;
begin
  list := TCnVarList.Create;
  list.fromString('((1,2,(1,2)),100,''abc'',true)', err);
//  list.fromString('((1,(2)),1,true)', err);
//  list.fromString('(3,''a'',(1, 2), true)', err);
//  list.FromString('(true)', err);
  print(list.toString());
  list.Free;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  list, list2: TCnVarList;
begin
  list := TCnVarList.Create;
  list2 := TCnVarList.Create;

  list2.setValues([1, 2]);
  list2.add(list2); // Ignored.
//  print(list2.ToString);
  list2.add(self);
  TForm1(list2.getObject(2)).Caption:=''; // Form

  list.add('''abc');
  list.add(list2);
//  list.remove(list.count-1);
  list.add(True);
  list.add(now);

  print(list.toString());

  list.Free;
  list2.Free;
end;

procedure TForm1.Print(Text: string);
begin
  Memo1.Lines.Append(Text);
end;

end.
