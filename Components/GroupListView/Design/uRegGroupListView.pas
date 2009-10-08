unit uRegGroupListView;

interface

uses
  Classes,
  uGroupListView;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('TUO',[TGroupListView]);
end;

end.
