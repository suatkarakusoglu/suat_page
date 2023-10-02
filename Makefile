.PHONY: publish-production-site

.generate-site:
	emacs --batch -l ./publish.el --funcall site/publish

generate-production-site:
	PRODUCTION=true make .generate-site

generate-local-site:
	PRODUCTION=false make .generate-site

generate-and-publish-production-site:
	git checkout gh-pages
	git merge --no-edit main
	make generate-production-site
	git add .
	git commit -m "Publish site."
	git push origin gh-pages
	git checkout main

see-production-site:
	open https://suat.page

see-local-site:
	open http://localhost:8080
