//Precond: "nombre" existe
JBSTNodeStr* find(string nombre, JBSTNodeStr* t){
	JBSTNodeStr* actual = t;
	while(current!=null&&current-<name!=name) {
		if(actual->name<nombre) {
			actual = actual->right;
		} else {actual = actual->left;}
	}
	return actual;
} 

//Precond: el arbol dado ya tiene "jefe" (nodo raíz)
void insertar(string nuevo, string superior, JerarquiaBST t) {
	JBSTNodeStr* nodoN = new JSBTNodeStr;
	nodoN->left = nullptr;
	nodoN->right = nullptr;
	nodoN->name = nuevo;
	nodoN->parent = find(superior,t->root);
	JBSTNodeStr* prev = nullptr;
	JBSTNodeStr* current= t->root;
	while(siguiente != nullptr) {
		prev=current;
		if (current->name>nuevo) {
			current=current->left;
		}
		else {current=current->right;}	
	}
	if (prev->name>nuevo) {
		prev->left=nodoN;
	}
	else {prev->right =nodoN;}
}


bool esSubordinadoDe(string empleado, string superior, JerarquiaBST t) {
	JBSTNodeStr* actual =find(emplead,t->root);
	while (actual->parent!=nullptr && current->name!=superior) {
		current = current->parent;
	}
	return current!=nullptr;
}

