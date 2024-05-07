# CODEFIT

O CODEFIT, é um sistema de gerenciamento de academia que pode ser utilizado por gestores, funcionários e alunos. O sistema contém funcionalidades específicas para cada usuário visando modularizar o sistema, atribuindo cada funcionalidade a entidade específica.



## Funcionalidades

- Conheça todas as funcionalidades disponíveis acessando a [documentação oficial do projeto](https://docs.google.com/document/d/1bcGVitOdJ7p6JWy1ikgXhyCYMsLPlYAsG7g90WwrG-o/edit#heading=h.phrzmz7sb5x0). 

- Vídeo de apresentação do CODEFIT:[Sistema CODEFIT]().


## Rodando localmente

### Cuidado!
É preciso que você tenha o SWI Prolog instalado e atualizado em sua máquina.
Caso não tenha o swi, 
  esse vídeo irá lhe ensinar detalhadamente [instalação swi](https://www.youtube.com/watch?v=YzDpQOk2qvQ).
E logo após, também, deve ser baixado uma pack, abaixo segue as instruções de como baixar:

Insira no terminal do seu computador
```bash
  swipl
```
Se estiver instalado, e aparecer ?-
```bash
  pack_install(smtp).
  // escolha o diretório e depois a letra Y
```

Tudo pronto? Vamos em frente!

##Para rodar as classes, siga as instruções abaixo

Clone o projeto em sua IDE

```bash
  git clone [https://github.com/kevinicolas22/projeto-plp-prolog.git]
```
Verifique se está na branch 'main', se não tiver insira : git checkout main

-> Entre no diretório do projeto

```bash
  cd prolog
```

-> Para rodar o menu principal

```bash
  swipl -q -f mainPrincipal.pl
```
-> Para rodar apenas gestor
```bash
  swipl -q -f mainGestor.pl
```
```bash
  menu_gestor(MenuPrincipal).
```
-> Para rodar apenas funcionario
```bash
  swipl -q -f mainFuncionario.pl 
```
```bash
  menu_funcionario.
```
-> Para rodar apenas aluno
```bash
  swipl -q -f mainAluno.pl
```
```bash
  login_aluno.
```

