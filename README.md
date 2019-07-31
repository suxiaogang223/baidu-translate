# baidu-translate
An emacs plugin using baidu-translate-api

##依赖
本插件需要unicode-escape插件的支持
你可以通过package-list-packages安装
也可以去 https://github.com/kosh04/unicode-escape.el 下载

##安装方法
将baidu-translate.el 下载到.emacs.d文件夹下
在.emacs 中添加

(load "~/.emacs.d/baidu-translate")
;;设置快捷键
(global-set-key (kbd "C-c m") 'baidu-translate-zh-mark)
(global-set-key (kbd "C-c M") 'baidu-translate-zh-whole-buffer)

申请百度翻译API，在baidu-translate.el中 defvar 处填写APPID和秘钥。

##翻译为其他语言
你可以通过源代码中的例子，查看百度翻译的语言列表，通过对baidu-translate-string 函数的包装，编写自己的UI函数，并设置快捷键。
例如：

(defun baidu-translate-zh-mark (start end)
  "Translate the marked text to Chinese."
  (interactive "r")
  (baidu-translate-string (buffer-substring start end) "auto" "zh"))

(defun baidu-translate-zh-whole-buffer ()
  "Translate the whole buffer to Chinese."
  (interactive)
  (baidu-translate-string (buffer-string) "auto" "zh"))

