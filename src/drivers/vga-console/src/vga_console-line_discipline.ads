with Text_Console.Line_Discipline;

with VGA_Console.Screen;

package VGA_Console.Line_Discipline is
  new Text_Console.Line_Discipline (Put => VGA_Console.Screen.Put);
