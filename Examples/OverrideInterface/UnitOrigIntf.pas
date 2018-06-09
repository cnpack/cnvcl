unit UnitOrigIntf;

interface

uses
  SysUtils, Classes, Windows;

type
  IOriginalNotifier = interface
  ['{4E3A8311-1D36-4C92-8A05-D388642B0038}']
    procedure OnNotify(Sender: TObject);
  end;

  IOriginalInterface = interface
  ['{EE690722-0845-4251-A48B-151526EDD74D}']
    procedure TestMethod(Data: Cardinal);
    function TestFunction(const S: string): Boolean;
    procedure AddNotifier(Notifier: IOriginalNotifier);
  end;

implementation

end.
