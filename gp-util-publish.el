(custom-set-variables
 '(org-html-preamble-format '(("en"
"
<div class=\"banner\">
<a id=\"name\">Google patents utility</a><br />
<a href=\"./about.html\"> about this site </a> | <a href=\"index.html\"> archive </a> | <a href=\"theindex.html\"> index </a></div>
<div class=\"title\"><h1>%t</h1></div>
<div class=\"date\">inventor<br />%a</div>
<br />
<div class=\"date\">filing date<br />%d</div>
"))))

(custom-set-variables
 '(org-html-postamble-format '(("en"  
"<div>
Confidential<br />
</div>
<script>
/**
*  RECOMMENDED CONFIGURATION VARIABLES: EDIT AND UNCOMMENT THE SECTION BELOW TO INSERT DYNAMIC VALUES FROM YOUR PLATFORM OR CMS.
*  LEARN WHY DEFINING THESE VARIABLES IS IMPORTANT: https://disqus.com/admin/universalcode/#configuration-variables*/
/*
var disqus_config = function () {
this.page.url = PAGE_URL;  // Replace PAGE_URL with your page's canonical URL variable
this.page.identifier = PAGE_IDENTIFIER; // Replace PAGE_IDENTIFIER with your page's unique identifier variable
};
*/
(function() { // DON'T EDIT BELOW THIS LINE
var d = document, s = d.createElement('script');
s.src = 'https://emacs-life.disqus.com/embed.js';
s.setAttribute('data-timestamp', +new Date());
(d.head || d.body).appendChild(s);
})();
</script>
"))))

(defvar my-html-head
  "<link rel=\"stylesheet\" href=\"./patent.css\" type=\"text/css\"/>")

(setq org-publish-project-alist
      `(
	("project" :components ("patents"))	
	("patents"
	 :base-directory "./"
	 :base-extension "org"
	 :publishing-directory "./"
	 :publishing-function org-html-publish-to-html
	 :section-numbers t
	 :with-author nil
	 :with-date nil
	 :with-toc t
	 :with-title nil
	 :html-validation-link nil
	 :html-head-include-default-style nil
	 :makeindex  nil
	 :auto-sitemap nil
;	 :sitemap-filename "index.org"
;	 :sitemap-title "Recent posts"
;	 :sitemap-format-entry org-publish-sitemap-my-entry
	 :html-head ,my-html-head
;	 :html-head-extra ,google-adsense
;	 :link-home "index.html"
	 :html-preamble t
	 :html-postamble t
	 )
	))

(global-set-key (kbd "\C-c p") 'org-publish)
(global-set-key (kbd "\C-c l") 'org-publish-blog-local)

