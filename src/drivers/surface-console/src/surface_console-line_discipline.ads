with Text_Console.Line_Discipline;

with Surface_Console.Screen;

package Surface_Console.Line_Discipline is
  new Text_Console.Line_Discipline (Put => Surface_Console.Screen.Put);
