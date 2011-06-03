" Menu Translations: Russian
" Maintainer:        Tim Alexeevsky <realtim [at] mail.ru>
" Last Change:       11 Nov 2003
" URL:               http://zigzag.cs.msu.su/~tim/menu_ru_ru.koi8-r.vim

"
" This translation is based on an ukrainian translation by
" Bohdan Vlasyuk <bohdan@vstu.edu.ua>
"

" Quit when menu translations have already been done.
if exists("did_menu_trans")
  finish
endif
let did_menu_trans = 1
scriptencoding koi8-r

" Help menu
menutrans &Help                                          &Помощь
menutrans &Overview<Tab><F1>                             &Обзор<Tab><F1>
menutrans &User\ Manual                                  &Пользовательское\ руководство
menutrans &How-to\ links                                 &Как\ сделать\.\.\.
"menutrans &GUI         &GIU
menutrans &Credits                                       &Благодарности
menutrans Co&pying                                       &Распространение
menutrans O&rphans                                       Помощь\ &сиротам
menutrans &Version                                       &Версия
menutrans &About                                         О\ &программе
" File menu
menutrans &File                                          &Файл
menutrans &Open\.\.\.<Tab>:e                             &Открыть\.\.\.<Tab>:e
menutrans Sp&lit-Open\.\.\.<Tab>:sp                      &Разделить\ окно\.\.\.<Tab>:sp
menutrans &New<Tab>:enew                                 &Новый<Tab>:enew
menutrans &Close<Tab>:close                              &Закрыть<Tab>:close
menutrans &Save<Tab>:w                                   &Сохранить<Tab>:w
menutrans Save\ &As\.\.\.<Tab>:sav                       Сохранить\ &как\.\.\.<Tab>:sav
menutrans Split\ &Diff\ with\.\.\.                       Ср&авнить\ с\.\.\.
menutrans Split\ Patched\ &By\.\.\.                      За&платать\.\.\.
menutrans &Print                                         &Распечатать
menutrans Sa&ve-Exit<Tab>:wqa                            Со&хранить\ и\ выйти<Tab>:wqa
menutrans E&xit<Tab>:qa                                  &Выход<Tab>:qa

" Edit menu
menutrans &Edit                                          &Правка
menutrans &Undo<Tab>u                                    &Отменить<Tab>u
menutrans &Redo<Tab>^R                                   В&ернуть<Tab>^R
menutrans Rep&eat<Tab>\.                                 &Повторить<Tab>\.
menutrans Cu&t<Tab>"+x                                   &Вырезать<Tab>"+x
menutrans &Copy<Tab>"+y                                  &Копировать<Tab>"+y
menutrans &Paste<Tab>"+P                                 В&ставить<Tab>"+p
menutrans Put\ &Before<Tab>[p                            Вставить\ п&еред<Tab>[p
menutrans Put\ &After<Tab>]p                             Вставить\ п&осле<Tab>]p
menutrans &Select\ all<Tab>ggVG                          В&ыделить\ все<Tab>ggVG
menutrans &Find\.\.\.                                    &Найти\.\.\.
menutrans Find\ and\ Rep&lace\.\.\.                      &Заменить\.\.\.
menutrans Settings\ &Window                              Окно\ наст&роек
menutrans &Global\ Settings                              Глобальные\ настро&йки
menutrans F&ile\ Settings                                Настройки\ для\ файла
menutrans Toggle\ Line\ &Numbering<Tab>:set\ nu!         \[Не\]\ &Нумеровать\ строки<Tab>:set\ nu!
menutrans Toggle\ &List\ Mode<Tab>:set\ list!            В\[ы\]ключить\ режим\ &списка<Tab>:set\ list!
menutrans Toggle\ Line\ &Wrap<Tab>:set\ wrap!            В\[ы\]ключить\ режим\ &переносов<Tab>:set\ wrap!
menutrans Toggle\ W&rap\ at\ word<Tab>:set\ lbr!         В\[ы\]ключить\ перенос\ слов\ &целиком<Tab>:set\ lbr!
menutrans Toggle\ &expand-tab<Tab>:set\ et!              \[Не\]\ Использовать\ символ\ &табуляции<Tab>:set\ et!
menutrans Toggle\ &auto-indent<Tab>:set\ ai!             В\[ы\]ключить\ автоматический\ &отступ<Tab>:set\ ai!
menutrans Toggle\ &C-indenting<Tab>:set\ cin!            В\[ы\]ключить\ отступы\ для\ языка\ &C<Tab>:set\ cin!
menutrans &Shiftwidth                                    &Автосдвиг
menutrans Te&xt\ Width\.\.\.                             &Ширина\ текста\.\.\.
menutrans &File\ Format\.\.\.                            &Формат\ файла\.\.\.
menutrans Soft\ &Tabstop                                 Позиция\ &табуляции
menutrans C&olor\ Scheme                                 &Цветовая\ схема
menutrans Select\ Fo&nt\.\.\.                            Выбрать\ &шрифт\.\.\.

menutrans &Keymap                                        Режим\ клавиатуры
menutrans Toggle\ Pattern\ &Highlight<Tab>:set\ hls!     \[Не\]\ Выделять\ &найденное<Tab>:set\ hls!
menutrans Toggle\ &Ignore-case<Tab>:set\ ic!             \[Не\]\ Различать\ регистр\ &букв<Tab>:set\ ic!
menutrans Toggle\ &Showmatch<Tab>:set\ sm!               \[Не\]\ Показывать\ &парную\ скобку<Tab>:set\ sm!
menutrans &Context\ lines                                Строчек\ &вокруг\ курсора
menutrans &Virtual\ Edit                                 Курсор\ ходит\ где\ хочет

menutrans Never                                          Никогда
menutrans Block\ Selection                               При\ выделении\ блока
menutrans Insert\ mode                                   В\ режиме\ вставки
menutrans Block\ and\ Insert                             И\ в\ том\ и\ в\ другом
menutrans Always                                         Всегда

menutrans Toggle\ Insert\ &Mode<Tab>:set\ im!            В\[ы\]ключить\ режим\ &вставки<Tab>:set\ im!
menutrans Toggle\ Vi\ C&ompatible<Tab>:set\ cp!          В\[ы\]ключить\ совместимость\ с\ Vi<Tab>:set\ cp!
menutrans Search\ &Path\.\.\.                            &Путь\ для\ поиска\.\.\.
menutrans Ta&g\ Files\.\.\.                              &Файлы\ тегов\.\.\.


"
" GUI options
menutrans Toggle\ &Toolbar                              \[Не\]\ Показывать\ панель\ &инструментов
menutrans Toggle\ &Bottom\ Scrollbar                    \[Не\]\ Показывать\ &полосу\ прокрутки\ снизу
menutrans Toggle\ &Left\ Scrollbar                      \[Не\]\ Показывать\ полосу\ прокрутки\ слева
menutrans Toggle\ &Right\ Scrollbar                     \[Не\]\ Показывать\ полосу\ прокрутки\ справа

" Programming menu
menutrans &Tools                                        &Инструменты
menutrans &Jump\ to\ this\ tag<Tab>g^]                  &Перейти\ на\ тег<Tab>g^]
menutrans Jump\ &back<Tab>^T                            Вернуться<Tab>^T
menutrans Build\ &Tags\ File                            Создать\ файл\ тегов
" Folding
menutrans &Folding                                      &Свертывание
menutrans &Enable/Disable\ folds<Tab>zi                 &Вкл/Выкл\ свертывание<Tab>zi
menutrans &View\ Cursor\ Line<Tab>zv                    Открыть\ строку\ с\ курсором<Tab>zv
menutrans Vie&w\ Cursor\ Line\ only<Tab>zMzx            Открыть\ только\ строку\ с\ курсором<Tab>zMzx
menutrans C&lose\ more\ folds<Tab>zm                    Свернуть\ больше<Tab>zm
menutrans &Close\ all\ folds<Tab>zM                     Свернуть\ все\ что\ можно<Tab>zM
menutrans &Open\ all\ folds<Tab>zR                      Открыть\ все<Tab>zR
menutrans O&pen\ more\ folds<Tab>zr                     Открыть\ больше<Tab>zr
menutrans Create\ &Fold<Tab>zf                          Создать\ сверток<Tab>zf
menutrans &Delete\ Fold<Tab>zd                          Удалить\ свертывание<Tab>zd
menutrans Delete\ &All\ Folds<Tab>zD                    Удалить\ все\ свертки<Tab>zD
menutrans Fold\ column\ &width                          Ширина\ колонки\ свертка
menutrans Fold\ Met&hod                                 Метод\ свертки
menutrans M&anual                                       &Ручной
menutrans I&ndent                                       &По\ отступу
menutrans E&xpression                                   По\ выражению
menutrans S&yntax                                       По\ синтаксису
menutrans Ma&rker                                       По\ маркерам

" Diff
menutrans &Diff                                         Diff
menutrans &Update                                       Обновить
menutrans &Get\ Block                                   Изменить\ этот\ буфер
menutrans &Put\ Block                                   Изменить\ другой\ буфер

" Make and stuff...
menutrans &Make<Tab>:make                               make<Tab>:make
menutrans &List\ Errors<Tab>:cl                         Список\ ошибок<Tab>:cl
menutrans L&ist\ Messages<Tab>:cl!                      Список\ предупреждений<Tab>:cl!
menutrans &Next\ Error<Tab>:cn                          Следующая\ ошибка<Tab>:cn
menutrans &Previous\ Error<Tab>:cp                      Предыдущая\ ошибка<Tab>:cp
menutrans &Older\ List<Tab>:cold                        Старые\ ошибки<Tab>:cold
menutrans N&ewer\ List<Tab>:cnew                        Более\ новые\ ошибки<Tab>:cnew
menutrans Error\ &Window                                Окно\ ошибок
menutrans &Update<Tab>:cwin                             Обновить<Tab>:cwin
menutrans &Close<Tab>:cclose                            &Закрыть<Tab>:cclose
menutrans &Open<Tab>:copen                              Открыть<Tab>:copen

menutrans &Set\ Compiler                                Задать\ компилятор
menutrans &Convert\ to\ HEX<Tab>:%!xxd                  Перевести\ в\ HEX<Tab>:%!xxd
menutrans Conve&rt\ back<Tab>:%!xxd\ -r                 Перевести\ обратно<Tab>:%!xxd\ -r

" Names for buffer menu.
menutrans &Buffers                                      &Буферы
menutrans &Refresh\ menu                                &Обновить
menutrans Delete                                        &Удалить
menutrans &Alternate                                    &Альтернативный
menutrans &Next                                         &Следующий
menutrans &Previous                                     &Предыдущий
menutrans [No\ File]                                    [Нет\ файла]

" Window menu
menutrans &Window                                       &Окно
menutrans &New<Tab>^Wn                                  &Новое<Tab>^Wn
menutrans S&plit<Tab>^Ws                                &Разделить<Tab>^Ws
menutrans Sp&lit\ To\ #<Tab>^W^^                        Открыть\ последний\ файл<Tab>^W^^
menutrans Split\ &Vertically<Tab>^Wv                    Разделить\ вертикально<Tab>^Wv
menutrans Split\ File\ E&xplorer                        Открыть\ директорию

menutrans &Close<Tab>^Wc                                &Закрыть<Tab>^Wc
menutrans Close\ &Other(s)<Tab>^Wo                      Закрыть\ остальные<Tab>^Wo
menutrans Ne&xt<Tab>^Ww                                 &Следующий<Tab>^Ww
menutrans P&revious<Tab>^WW                             &Предыдущий<Tab>^WW
menutrans &Equal\ Size<Tab>^W=                          &Выровнять\ размер<Tab>^W=
menutrans &Max\ Height<Tab>^W_                          Максимальная\ высота<Tab>^W_
menutrans M&in\ Height<Tab>^W1_                         Минимальная\ высота<Tab>^W1_
menutrans Max\ &Width<Tab>^W\|                          Максимальная\ ширина<Tab>^W\|
menutrans Min\ Widt&h<Tab>^W1\|                         Минимальная\ ширина<Tab>^W1\|
menutrans Move\ &To                                     &Переместить
menutrans &Top<Tab>^WK                                  Вверх<Tab>^WK
menutrans &Bottom<Tab>^WJ                               Вниз<Tab>^WJ
menutrans &Left\ side<Tab>^WH                           Влево<Tab>^WH
menutrans &Right\ side<Tab>^WL                          Вправо<Tab>^WL
menutrans Rotate\ &Up<Tab>^WR                           &Сдвинуть\ вверх<Tab>^WR
menutrans Rotate\ &Down<Tab>^Wr                         Сдвинуть\ вниз<Tab>^Wr

" The popup menu
menutrans &Undo                                         Отменить
menutrans Cu&t                                          Вырезать
menutrans &Copy                                         &Копировать
menutrans &Paste                                        Вставить
menutrans &Delete                                       Удалить
menutrans Select\ &Word                                 Выбрать\ &слово
menutrans Select\ &Line                                 Выбрать\ &строку
menutrans Select\ &Block                                Выбрать\ &блок
menutrans Select\ &All                                  Выбрать\ &все

" Syntax menu
menutrans &Syntax                                       &Синтаксис
menutrans Set\ '&syntax'\ only                          Установить\ только\ '&syntax'
menutrans Set\ '&filetype'\ too                         Установить\ также\ '&filetype'
menutrans &Off                                          &Выключить
menutrans &Manual                                       &Ручной
menutrans A&utomatic                                    &Автоматический
menutrans on/off\ for\ &This\ file                      Включить/выключить\ для\ этого\ файла
menutrans Co&lor\ test                                  Проверка\ цветов
menutrans &Highlight\ test                              Проверка\ подсветки
menutrans &Convert\ to\ HTML                            Сделать\ &HTML

" The GUI toolbar
if has("toolbar")
  if exists("*Do_toolbar_tmenu")
    delfun Do_toolbar_tmenu
  endif
  fun Do_toolbar_tmenu()
    tmenu ToolBar.Open                                  Открыть файл
    tmenu ToolBar.Save                                  Сохранить файл
    tmenu ToolBar.SaveAll                               Сохранить все файлы
    tmenu ToolBar.Print                                 Распечатать
    tmenu ToolBar.Undo                                  Отменить
    tmenu ToolBar.Redo                                  Вернуть
    tmenu ToolBar.Cut                                   Вырезать
    tmenu ToolBar.Copy                                  Копировать
    tmenu ToolBar.Paste                                 Вставить
    tmenu ToolBar.Find                                  Найти...
    tmenu ToolBar.FindNext                              Найти следующий
    tmenu ToolBar.FindPrev                              Найти предыдущий
    tmenu ToolBar.Replace                               Заменить...
    tmenu ToolBar.LoadSesn                              Загрузить сеанс редактирования
    tmenu ToolBar.SaveSesn                              Сохранить сеанс редактирования
    tmenu ToolBar.RunScript                             Запустить скрипт
    tmenu ToolBar.Make                                  Make
    tmenu ToolBar.Shell                                 Shell
    tmenu ToolBar.RunCtags                              Создать файл тегов
    tmenu ToolBar.TagJump                               Перейти по тегу
    tmenu ToolBar.Help                                  Помощь
    tmenu ToolBar.FindHelp                              Найти подсказку
  endfun
endif

" dialog texts
let menutrans_no_file = "[Нет файлов]"
let menutrans_help_dialog = "Введите команду или слово для поиска:\nДобавьте i_ для поиска команд режима вставки (напр. i_CTRL-X)\nДобавьте c_ для команд командного режима (напр. с_<Del>)\nДобавьте ' для поиска опции (напр. 'shiftwidth')"
let g:menutrans_path_dialog = "Укажите путь для поиска файлов\nНазвания директоий разделяйте запятыми."
let g:menutrans_tags_dialog = "Введите имена файлов тегов\nРазделяйте имена запятыми."
let g:menutrans_textwidth_dialog = "Введите ширину текста для форматирования.\n0 для отмены форматирования"
let g:menutrans_fileformat_dialog = "Выберите формат файла"

