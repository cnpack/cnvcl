unit UnitOrigIntf;

interface

uses
  SysUtils, Classes, Windows;

type
  IOriginalInterface = interface
  ['{EE690722-0845-4251-A48B-151526EDD74D}']
    procedure TestMethod(Data: Cardinal);
    function TestFunction(const S: string): Boolean;
  end;

implementation

end.
