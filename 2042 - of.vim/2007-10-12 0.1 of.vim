function OF()
ruby << EOF
require 'find'
require 'curses'
require 'sqlite3'

module Curses
   def self.program
      main_screen = init_screen
      noecho
      cbreak
      curs_set(0)
      main_screen.keypad = true
      yield main_screen
   end
end

include Curses
include SQLite3

@items = []
@items_archive = {}
@selected = 1
@offset = 0
@max_files = stdscr.maxy - 2
@col = 0
@dd = ""
@d = 0
@found_once = false
@modeline = '*enter at least 2 chars* (current: ' + File.basename($curbuf.name.to_s) + ')'
@db = Database.new(ENV['HOME'] + '/.of.db')
@buffers = []

def init_db
   @db.execute('CREATE TABLE IF NOT EXISTS searches (sel_path TEXT, search_str TEXT, pwd TEXT, time INTEGER, PRIMARY KEY(search_str, pwd))')
   @db.execute('CREATE TABLE IF NOT EXISTS recent_files (sel_path TEXT, time INTEGER, PRIMARY KEY(sel_path))')
   @searches = @db.execute("SELECT search_str, sel_path FROM searches WHERE pwd = '" + Dir.pwd + "' ORDER BY time DESC")
   @recent_files = @db.execute("SELECT sel_path FROM recent_files ORDER BY time DESC LIMIT 20")
   @searches = Hash[*@searches.collect { |v| [v, v*2] }.flatten]
   if !@recent_files.empty? and File.exist? @recent_files[0][0] and $curbuf.name != @recent_files[0][0]
      @last_file = @recent_files[0][0]
   elsif !@recent_files.empty? and File.exist? @recent_files[1][0] and $curbuf.name != @recent_files[1][0]
      @last_file = @recent_files[1][0]
   else
      @last_file = ''
   end
end

def make_regex(str)
   regex = ''
   str.each_byte do |p|
      p = p.chr
      if ['.', '[', ']'].include? p
         p = '\\' + p
      end
      regex += p + '.*?'
   end
   Regexp.new(regex)
end

def load_open_buffs
   @items = @buffers + @searches.values
   @found_once = false
   @modeline = 'switch buffer: (current: ' + File.basename($curbuf.name.to_s) + ')'
   uniq_items
end

def find_files(regex)
   if @items_archive[@dd]
      @items = @items_archive[@dd]
   else
      Find.find(Dir.pwd) do |path|
         if FileTest.directory?(path)
            if File.basename(path)[0] == ?.
               Find.prune
            else
               next
            end
         elsif File.basename(path)[-4,4] == '.swp' 
            Find.prune
         elsif File.basename(path).match(regex)
            @items.push(path)
         end
      end
      find_in_items(regex)
      @items_archive[@dd] = @items
   end
end

def find_in_items(regex)
   if @items_archive[@dd]
      @items = @items_archive[@dd]
   else
      @items.delete_if do |item|
         !File.basename(item).match(regex)
      end
      uniq_items
      @items_archive[@dd] = @items
   end
   @selected = 1
end

def uniq_items
   @items.uniq!
   if @last_file != ''
      @items.delete @last_file
      @items.unshift @last_file
   end
   @items.each do |i|
      if File.basename(i) == @dd
         @items.delete i
         @items.unshift i
      end
   end
   if @dd != '' and @searches[@dd]
      @items.delete @searches[@dd]
      @items.unshift @searches[@dd]
   end
   #@items = @items - [$curbuf.name]
end

def fmt_str(str)
   if !@items.empty?
      items = @items.find_all {|it| File.basename(it) == File.basename(str)}
   else
      items = []
   end
   if items.size > 1
      return " (" + find_uniq(items, str) + ")"
   else
      return ""
   end
end

def find_uniq(items, str)
   items = items - [str]
   File.dirname(str).split("/").reverse.each_with_index do |item, ind|
      if (items.inject(true) {|val, it| val = (it.split("/").reverse[ind + 1] != item) && val})
         return item
      end
   end
   return File.dirname(str)
end

def update_screen
   setpos(0,0)
   clrtoeol
   addstr(@dd)
   setpos(1,0)
   clrtoeol
   addstr(@modeline)
   count = 1
   offset_count = 0
   @items.each do |i|
      if offset_count >= @offset
         if count <= @max_files
            setpos(count + 1, 0)
            clrtoeol
            if count == stdscr.maxy - 2 and @items.size > @max_files and @offset + @max_files < @items.size
               addstr("+ ")
            end
            if count == 1 and @items.size > @max_files and @offset > 0
               addstr("- ")
            end
            setpos(count + 1, 2)
            if @buffers.include? i
               attron A_BOLD
            end
            if count + @offset == @selected
               attron(color_pair(COLOR_BLUE))
               addstr(File.basename(i))
               addstr(fmt_str(i))
               attroff(color_pair(COLOR_BLUE))
            else
               attron(color_pair(COLOR_CYAN))
               addstr(File.basename(i))
               attroff(color_pair(COLOR_CYAN))
               addstr(fmt_str(i))
            end
            attroff A_BOLD
            count += 1
         end
      end
      offset_count += 1
   end
   setpos(0, @col)
end

def select_item
   @db.execute("UPDATE searches SET sel_path = ?, time = datetime('now', 'localtime') WHERE search_str = ? AND  pwd = ?", @items[@selected - 1], @dd, Dir.pwd)
   if @db.total_changes < 1
      @db.execute("INSERT INTO searches VALUES (?, ?, ?, datetime('now', 'localtime'))", @items[@selected - 1], @dd, Dir.pwd)
   end
   changes = @db.total_changes
   @db.execute("UPDATE recent_files SET time = datetime('now', 'localtime') WHERE sel_path = ?", @items[@selected - 1])
   if changes == @db.total_changes
      @db.execute("INSERT INTO recent_files VALUES (?, datetime('now', 'localtime'))", @items[@selected - 1])
   end
   VIM::command('e! ' + @items[@selected - 1])
end

def move_down
   @selected += 1 unless @selected == @items.size
   if @selected > @offset + @max_files
      scroll_down
   end
end

def move_up
   @selected -= 1 unless @selected == 1
   if @selected <= @offset
      scroll_up
   end
end

def scroll_down
   @offset += 1 unless @offset + @max_files == @items.size
   if @selected <= @offset
      @selected = @offset + 1
   end
end

def scroll_up
   @offset -= 1 unless @offset == 0
   if @selected > @max_files + @offset
      @selected = @max_files + @offset
   end
end

Curses.program do
   init_db
   VIM::Buffer.count.times {|b| @buffers.push VIM::Buffer[b].name unless VIM::Buffer[b].name == nil}
   load_open_buffs
   main_screen = init_screen
   start_color
   clear
   setpos(0,0)
   init_pair(COLOR_BLUE, COLOR_BLUE, COLOR_BLACK)
   init_pair(COLOR_CYAN, COLOR_CYAN, COLOR_BLACK)
   noecho
   trap(0) { echo }
   update_screen
   waited = false
   found_str = ""
   stdscr.nodelay = true
   while (c = stdscr.getch) do
      case c
         when 27
            break
         when 127, 263
            @dd.chop!
            @col -= 1 unless @col == 0
            update_screen
            refresh
            if @col == 0
               load_open_buffs
               waited = false
               found_str = ""
            else
               regex = make_regex(@dd)
               find_files(regex)
               @found_once = true
               found_str = @dd
            end
         when 10
            select_item
            break
         when 21
            @col = 0
            @dd = ""
            waited = false
            found_str = ""
            load_open_buffs
         when 258, 14
            move_down
         when 259, 16
            move_up
         when Curses::KEY_MOUSE
            c = stdscr.getch
            if c == 97
               scroll_down
            elsif c == 96
               scroll_up
            elsif c == 32
               x = stdscr.getch.to_i - 33
               y = stdscr.getch.to_i - 33
               if y > 1 and x > 1 and File.basename(@items[y - 2 + @offset]).length > x - 2
                  @selected = y - 1 + @offset
                  select_item
                  c = stdscr.getch
                  c = stdscr.getch
                  c = stdscr.getch
                  break
               else
                  c = stdscr.getch
               end
            end
            c = stdscr.getch
            c = stdscr.getch
#         when 260
#            @col -= 1 unless @col == 0
#         when 261
#            @col += 1 unless @col == @dd.length
         else 
            if c < 255 and c.chr.match /[a-zA-Z0-9.-_\[\] ]/
               @dd += c.chr
               @col += 1
               waited = false
               update_screen
               refresh
               regex = make_regex(@dd) if @dd.length > 1
            else
               if @dd.length > 1
                  if @found_once and found_str != @dd
                     if waited or @items_archive[@dd]
                        find_in_items(regex)
                        found_str = @dd
                        @modeline = 'choose file:'
                     else
                        sleep 0.1
                        waited = true
                     end
                  else
                     if waited
                        find_files(regex)
                        @found_once = true
                        found_str = @dd
                        @modeline = 'choose file:'
                     else
                        sleep 0.3
                        waited = true
                     end
                  end
                  update_screen
                  refresh
               else
                  @modeline = '*enter at least 2 chars* (current: ' + File.basename($curbuf.name.to_s) + ')'
                  update_screen
                  refresh
               end
            end
      end
      update_screen
   end

   @db.close
   VIM::command('redraw!')
end

EOF
endfunction
