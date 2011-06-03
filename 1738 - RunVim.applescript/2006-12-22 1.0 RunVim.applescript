
-- Run command: "vim; exit"
on open untitled obj
  tell application "Terminal"
    do script "vim; exit"
    activate
  end
  quit application
end

-- Run command: "cd '/posix/path'; vim -- 'f1' 'f2'; exit"
on open alias_list
  set a to item 1 of alias_list
  set text item delimiters to ":"
  set list1 to text items of (a as string)
  set dir to (items 1 thru -2 of list1) as string
  set dir to quoted form of POSIX path of dir
  set list1 to {}
  repeat with a in alias_list
    set n to quoted form of name of (info for a)
    set end of list1 to n
  end
  set text item delimiters to " "
  set cat to list1 as text
  tell application "Terminal"
    do script "cd " & dir & "; vim -- " & cat & "; exit"
    activate
  end
  quit application
end

