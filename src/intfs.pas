unit intfs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  IDocument = interface ['{33F35F97-56E0-44FD-A0AA-5EA0CAD1C6B5}']
    procedure OpenFile(AFileName: string);
  end;


implementation

end.

