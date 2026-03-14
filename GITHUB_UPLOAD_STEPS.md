# 上传到 GitHub 的步骤 | Steps to Upload to GitHub

本地仓库已初始化，所有文件已 `git add`。按下面步骤完成首次提交并推送到 GitHub。

---

## 第一步：配置 Git 用户信息（若尚未配置）

在终端执行（把邮箱和名字换成你的）：

```bash
git config --global user.email "your-email@example.com"
git config --global user.name "Your Name"
```

若只想在本仓库使用，可去掉 `--global`，在 `github_code` 目录下执行：

```bash
cd "/Users/taozhu/my researches/lancet_financial/github_code"
git config user.email "your-email@example.com"
git config user.name "Your Name"
```

---

## 第二步：提交到本地仓库

```bash
cd "/Users/taozhu/my researches/lancet_financial/github_code"
git add .
git commit -m "Initial commit: GHE-life expectancy replication code and data"
```

---

## 第三步：在 GitHub 上创建新仓库

1. 打开 https://github.com/new
2. **Repository name**：例如 `ghe-life-expectancy` 或 `lancet-ghe-replication`
3. **Description**（可选）：`Replication code for GHE-life expectancy threshold analysis (Lancet Global Health)`
4. 选择 **Public**
5. **不要**勾选 “Add a README / .gitignore / license”（本地已有）
6. 点击 **Create repository**

---

## 第四步：添加远程仓库并推送

创建好后，GitHub 会显示类似命令。在终端执行（把 `YOUR_USERNAME` 和 `YOUR_REPO` 换成你的用户名和仓库名）：

```bash
cd "/Users/taozhu/my researches/lancet_financial/github_code"
git remote add origin https://github.com/YOUR_USERNAME/YOUR_REPO.git
git branch -M main
git push -u origin main
```

**若使用 SSH：**

```bash
git remote add origin git@github.com:YOUR_USERNAME/YOUR_REPO.git
git branch -M main
git push -u origin main
```

---

## 常见问题

- **推送时要求登录**：HTTPS 会提示 GitHub 用户名和密码；密码处需使用 **Personal Access Token**（Settings → Developer settings → Personal access tokens）。
- **仓库体积**：当前约 150MB，若 GitHub 提示超过 100MB 单文件限制，需要从仓库中移除或使用 Git LFS 处理大文件。
- **分支名**：本地已使用 `main`，与 GitHub 默认一致。

完成以上步骤后，代码会出现在你的 GitHub 仓库中。
