#!/usr/bin/env ruby

require 'optparse'
require File.join(File.dirname(__FILE__), 'keyboard_data')
require File.join(File.dirname(__FILE__), 'emacs_history')

Version = "0.0.1"

class EmacsKeybind
  def initialize(keyboard, filter, history)
    # キーバインドデータ
    src = []

    while line = gets
      src << line.chomp.split(/\t+/)
    end

    @keybind = src

    # キーボードデータ
    @keyboard = keyboard

    # フィルター名
    @filter = filter

    # 履歴データ
    @history = history
  end

  def functions(regexp)
    funcs = []
    @keybind.each do |data|
      funcs << data if data[0] =~ regexp
    end
    funcs
  end

  def print_keybind(title, prefix, suffix, link_name='')
    puts %Q{<h3><a name=\"#{link_name}\">#{title}</a></h3>} if (@filter == 'html')
    puts %Q{*** #{title}} if (@filter == 'hatena')

    puts %Q{<table cellspacing="2" cellpadding="0" border="1">}
    @keyboard.each do |row|
      puts %Q{<tr>}
      row.each do |key|
        funcs = []

        if (key.size == 2)
          key[1].each do |s|
            f = functions(Regexp.new("#{prefix}#{s}#{suffix}"))
            f.each {|a| a[1] += "(#{s.sub(/\\/, '')})" if (a[1] !~ /\(.\)$/) }
            funcs.concat f
          end
        end

        puts %Q{<td #{td_attr(key, funcs)}><p align="center">#{p_value(key, funcs)}</p></td>}
      end
      puts %Q{</tr>}
    end
    puts %Q{</table>}
  end

  def td_attr(key, funcs)
    attr = []
    color = '#80ff00'
    color = @history.color(funcs[0][1].sub(/\(.*\)$/, '')) if (!funcs.empty? and @history)
    attr << "bgcolor=\"#{color}\"" if (funcs.size > 0)
    attr.join(' ')
   end

  def p_value(key, funcs)
    str = "#{key[0]}"

    funcs.each do |func|
      str += "<br><b><font size=\"-1\" color=\"#7F00FF\">#{func[1]}</font></b>"
    end

    str
  end

  def print_standard_keybind
    print_keybind("?", '^', '$', 'self')
    print_keybind("C-?", '^C-', '$', 'control')
    print_keybind("M-?", '^M-', '$', 'alt')
    print_keybind("C-M-?", '^C-M-', '$', 'control_alt')
    print_keybind("C-x C-?", '^C-x C-', '$', 'control_x_control')
    print_keybind("C-x ?", '^C-x ', '$', 'control_x')
    print_keybind("C-x 4 ?", '^C-x 4 ', '$', 'control_x_4')
    print_keybind("C-x 5 ?", '^C-x 5 ', '$', 'control_x_5')
    print_keybind("C-x a ?", '^C-x a ', '$', 'control_x_a')
    print_keybind("C-x n ?", '^C-x n ', '$', 'control_x_n')
    print_keybind("C-x r ?", '^C-x r ', '$', 'control_x_r')
    print_keybind("C-x v ?", '^C-x v ', '$', 'control_x_v')
    print_keybind("C-c C-?", '^C-c C-', '$', 'control_c_control')
    print_keybind("C-c ?", '^C-c ', '$', 'control_c')
  end
end

if __FILE__ == $0
  keyboard_kind = nil
  filter = 'html'
  history_file_name = nil

  opt = OptionParser.new('Usage: emacs_keybind [options] input_file')
  opt.on('-k', '--keyboard (ascii, japanese)') {|v| keyboard_kind = v }
  opt.on('-f', '--filter (html, hatena)') {|v| filter = v }
  opt.on('-h', '--history HISTORY_FILE_NAME') {|v| history_file_name = v }
  opt.parse!(ARGV)

  case keyboard_kind
  when 'ascii'
    keyboard = KeyboardAscii
  when 'japanese'
    keyboard = KeyboardJapanese
  else
    if (keyboard_kind)
      abort "#{keyboard_kind} is not support. please set keyboard kind(ascci or japanese)"
    else
      abort "please set keyboard kind(ascci or japanese)"
    end
  end

  case filter
  when 'html'
  when 'hatena'
  else
    abort "#{keyboard_kind} is not support. please set keyboard kind(ascci or japanese)"
  end

  # セットアップ
  history = EmacsHistory.new(history_file_name) if (history_file_name)
  keybind = EmacsKeybind.new(keyboard, filter, history)

  # HTML出力
  print Header if (filter == 'html')
  print HeaderHatena if (filter == 'hatena')

  if (history)
    puts %Q{<h1><a name="frequency">Frequency in use</a></h3>}

    puts %Q{<table cellspacing="2" cellpadding="0" border="1">}
    puts %Q{<tr><td><b>Ranking</b></td><td><b>Use Count</b></td><td><b>Emacs Command</b></td></tr>}
    history.ranking.each do |a|
      puts %Q{<tr><td>#{a[1].ranking}</td><td>#{a[1].use_count}</td><td bgcolor=#{history.color(a[0])}>#{a[0]}</td></tr>}
    end
    puts %Q{</table>}
  end
  
  puts %Q{<h1><a name="keybind">Keybind</a></h1>}
  keybind.print_standard_keybind

  print Footer if (filter == 'html')
end
